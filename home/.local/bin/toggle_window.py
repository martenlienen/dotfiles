#!/usr/bin/env python3

import random
import shutil
import subprocess
import tempfile
from argparse import ArgumentParser
from datetime import datetime
from pathlib import Path


def dbus_send(*args):
    return subprocess.run(
        [
            "dbus-send",
            "--session",
            "--dest=org.kde.KWin",
            "--print-reply=literal",
            *args,
        ],
        capture_output=True,
        text=True,
        check=True,
    )


def kwin_run_script(script: str):
    script_name = "toggle_window.py-" + str(random.getrandbits(10))
    with tempfile.TemporaryDirectory() as d:
        script_path = Path(d) / "script.js"
        script_path.write_text(script)

        # install the script
        install_proc = dbus_send(
            "/Scripting",
            "org.kde.kwin.Scripting.loadScript",
            f"string:{script_path}",
            f"string:{script_name}",
        )
        script_id = int(install_proc.stdout.strip().split(" ")[1])
        dbus_send(f"/Scripting/Script{script_id}", "org.kde.kwin.Script.run")
        dbus_send(f"/Scripting/Script{script_id}", "org.kde.kwin.Script.stop")
        dbus_send(
            "/Scripting", "org.kde.kwin.Scripting.unloadScript", f"string:{script_name}"
        )


def window_exists(
    window_name: str | None, window_class: str | None, window_caption: str | None
):
    script = """
let window_name = WINDOW_NAME;
let window_class = WINDOW_CLASS;
let window_caption = new RegExp(WINDOW_CAPTION);
let target_window = null;
for (let window of workspace.stackingOrder) {
    if (window.resourceName != window_name && window.resourceClass != window_class) {
        continue;
    }
    if (!window_caption.test(window.caption)) {
        continue;
    }

    target_window = window;
    break;
}

if (target_window !== null) {
    console.log("WINDOW_EXISTS")
} else {
    console.log("WINDOW_MISSING")
}
"""

    if window_name is None:
        script = script.replace("WINDOW_NAME", "null")
    else:
        script = script.replace("WINDOW_NAME", f'"{window_name}"')
    if window_class is None:
        script = script.replace("WINDOW_CLASS", "null")
    else:
        script = script.replace("WINDOW_CLASS", f'"{window_class}"')
    if window_caption is None:
        script = script.replace("WINDOW_CAPTION", "")
    else:
        script = script.replace("WINDOW_CAPTION", f'"{window_caption}"')

    since = datetime.now()
    kwin_run_script(script)
    cmd = ["journalctl", "_COMM=kwin_wayland", "-o", "cat", "--since", str(since)]
    script_out = subprocess.run(cmd, check=True, capture_output=True, text=True)
    return "WINDOW_EXISTS" in script_out.stdout


def start_program(program, args: list[str]):
    path = shutil.which(program)
    subprocess.Popen([path] + args, start_new_session=True)


def toggle_program(
    window_name: str | None, window_class: str | None, window_caption: str | None
):
    toggle_script_template = """
function rects_overlap(rect_a, rect_b) {
    return !(rect_a.x + rect_a.width <= rect_b.x ||
             rect_b.x + rect_b.width <= rect_a.x ||
             rect_a.y + rect_a.height <= rect_b.y ||
             rect_b.y + rect_b.height <= rect_a.y);
}

function window_is_covered(window, desktop) {
    let i = 0;
    // Skip over all windows that are definitely below the given window
    while (i < workspace.stackingOrder.length && workspace.stackingOrder[i] != window) {
        i += 1;
    }
    i += 1
    // Iterate over the windows that could be above
    for (; i < workspace.stackingOrder.length; i++) {
        let other = workspace.stackingOrder[i];
        if (!(other.desktops.includes(desktop))) {
            continue;
        }
        if (other.minimized) {
            // Minimized windows keep their place in the stacking order even though they
            // are not visible
            continue;
        }
        if (rects_overlap(other.frameGeometry, window.frameGeometry)) {
            return true;
        }
    }
    return false;
}

let window_name = WINDOW_NAME;
let window_class = WINDOW_CLASS;
let window_caption = new RegExp(WINDOW_CAPTION);
let target_window = null;
for (let window of workspace.stackingOrder) {
    if (window.resourceName != window_name && window.resourceClass != window_class) {
        continue;
    }
    if (!window_caption.test(window.caption)) {
        continue;
    }

    target_window = window;
    break;
}

if (target_window !== null) {
    let output_geometry = workspace.activeScreen.geometry;
    if (rects_overlap(output_geometry, target_window.frameGeometry) && !target_window.minimized && !window_is_covered(target_window, workspace.currentDesktop)) {
        target_window.minimized = true;
    } else {
        // Center the window
        target_window.frameGeometry = {
            x: output_geometry.x + (output_geometry.width / 2) - (target_window.width / 2),
            y: output_geometry.y + (output_geometry.height / 2) - (target_window.height / 2),
            width: target_window.width,
            height: target_window.height,
        }

        target_window.desktops = workspace.currentDesktop;
        target_window.minimized = false;
        workspace.raiseWindow(target_window);
    }
}
"""

    toggle_script = toggle_script_template
    if window_name is None:
        toggle_script = toggle_script.replace("WINDOW_NAME", "null")
    else:
        toggle_script = toggle_script.replace("WINDOW_NAME", f'"{window_name}"')
    if window_class is None:
        toggle_script = toggle_script.replace("WINDOW_CLASS", "null")
    else:
        toggle_script = toggle_script.replace("WINDOW_CLASS", f'"{window_class}"')
    if window_caption is None:
        toggle_script = toggle_script.replace("WINDOW_CAPTION", "")
    else:
        toggle_script = toggle_script.replace("WINDOW_CAPTION", f'"{window_caption}"')
    kwin_run_script(toggle_script)


def main():
    parser = ArgumentParser(description="Start or toggle a program")
    parser.add_argument("--name", dest="window_name", help="WM_NAME of the window")
    parser.add_argument("--class", dest="window_class", help="WM_CLASS of the window")
    parser.add_argument(
        "--caption", dest="window_caption", help="Regexp matching the window title"
    )
    parser.add_argument("program", nargs="+", help="Program and options")
    args = parser.parse_args()

    program, *program_args = args.program
    window_name = args.window_name
    window_class = args.window_class
    window_caption = args.window_caption

    if window_name is None:
        window_name = program
    if window_class is None:
        window_class = program.title()

    if window_exists(window_name, window_class, window_caption):
        toggle_program(window_name, window_class, window_caption)
    else:
        start_program(program, program_args)


if __name__ == "__main__":
    main()
