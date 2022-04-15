#!/usr/bin/env python3

import shutil
import subprocess
from argparse import ArgumentParser

APP_SIZE = {"todoist": (50, 85), "spotify": (75, 75)}
CURRENT_DESKTOP = None


def current_desktop():
    global CURRENT_DESKTOP

    if CURRENT_DESKTOP is None:
        result = subprocess.run(
            ["xdotool", "get_desktop"],
            timeout=1,
            text=True,
            capture_output=True,
            check=True,
        )

        CURRENT_DESKTOP = int(result.stdout)

    return CURRENT_DESKTOP


def start_app(app):
    path = shutil.which(app)
    subprocess.Popen([path], start_new_session=True)


def window_is_visible(window_id: str):
    result = subprocess.run(
        ["xdotool", "get_desktop_for_window", window_id],
        check=False,
        timeout=1,
        text=True,
        capture_output=True,
    )

    # Window not visible on any desktop
    if result.returncode != 0:
        return False

    return int(result.stdout) == current_desktop()


def show_app(window_id: str, app: str):
    width, height = APP_SIZE[app]
    left, top = (100 - width) / 2, (100 - height) / 2

    subprocess.run(
        [
            "xdotool",
            "windowmap",
            "--sync",
            window_id,
            "windowsize",
            "--sync",
            window_id,
            f"{width}%",
            f"{height}%",
            "windowmove",
            "--sync",
            window_id,
            f"{left}%",
            f"{top}%",
            "set_desktop_for_window",
            window_id,
            str(current_desktop()),
            "windowraise",
            window_id,
        ],
        check=False,
        timeout=1,
        text=True,
        capture_output=True,
    )


def hide_app(window_id: str):
    subprocess.run(
        ["xdotool", "windowunmap", "--sync", window_id],
        check=False,
        timeout=1,
        text=True,
        capture_output=True,
    )


def get_todoist_window_id():
    # Todoist has weird hidden windows, which we need to exclude
    result = subprocess.run(
        ["xdotool", "search", "--class", "todoist"],
        check=False,
        timeout=1,
        text=True,
        capture_output=True,
    )
    all_todoist = set(result.stdout.splitlines())
    result = subprocess.run(
        ["xdotool", "search", "--role", "browser-window"],
        check=False,
        timeout=1,
        text=True,
        capture_output=True,
    )
    todoist = set(result.stdout.splitlines()) & all_todoist

    if len(todoist) > 0:
        return list(todoist)[0]
    else:
        return None


def get_window_id(app: str):
    result = subprocess.run(
        ["xdotool", "search", "--class", app],
        check=False,
        timeout=1,
        text=True,
        capture_output=True,
    )
    windows = result.stdout.splitlines()
    if len(windows) == 1:
        return windows[0]
    else:
        return None


def main():
    parser = ArgumentParser(description="Toggle an app on and off")
    parser.add_argument("--class", dest="window_class", help="WM_CLASS of the window")
    parser.add_argument("app")
    args = parser.parse_args()

    app = args.app
    window_class = args.window_class

    if app == "todoist":
        window_id = get_todoist_window_id()
    else:
        window_id = get_window_id(app)

    if window_id is None:
        start_app(app)
        return

    if window_is_visible(window_id):
        hide_app(window_id)
    else:
        show_app(window_id, app)


if __name__ == "__main__":
    main()
