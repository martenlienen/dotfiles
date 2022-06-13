#!/usr/bin/env python3

import json
from pathlib import Path
import shutil
import subprocess
from argparse import ArgumentParser

I3MSG_PATH = "/usr/bin/i3-msg"


def i3_layout_tree():
    tree_cmd = [I3MSG_PATH, "-t", "get_tree"]
    tree_result = subprocess.run(tree_cmd, stdout=subprocess.PIPE)

    if tree_result.returncode != 0:
        raise Exception("Could not get i3's layout tree")
    else:
        return json.loads(tree_result.stdout)


class WorkspaceException(Exception):
    """Yes, this is abused for a non-local return."""

    def __init__(self, workspace):
        self.workspace = workspace


def find_program_workspace(window_class):
    def search(node, workspace=None):
        if node.get("type") == "workspace":
            workspace = node

        class_name = node.get("window_properties", {}).get("class")
        if class_name is not None and class_name.lower() == window_class.lower():
            raise WorkspaceException(workspace)
        else:
            if "nodes" in node:
                for child in node["nodes"]:
                    search(child, workspace)
            if "floating_nodes" in node:
                for child in node["floating_nodes"]:
                    search(child, workspace)

    try:
        layout_tree = i3_layout_tree()
        search(layout_tree)

        raise Exception(f"{window_class} window not found in the layout tree")
    except WorkspaceException as e:
        return e.workspace


def program_runs(program):
    if program == "mattermost-desktop":
        # Mattermost is an electron application which makes it a bit harder to
        # determine, if it is running. We manually check all running processes if any of
        # them look like mattermost.
        root = Path("/proc")
        for proc_dir in root.iterdir():
            cmdline_f = proc_dir / "cmdline"
            if cmdline_f.is_file():
               cmdline = cmdline_f.read_text()
               if "electron" in cmdline and "mattermost-desktop" in cmdline:
                   return True
        return False
    else:
        # We match against the process name, so that we don't match toggle_window
        # itself. However, the process name is at most 16 null-terminated bytes [1], so
        # we only search for the first 15 characters.
        #
        # [1] https://stackoverflow.com/questions/23534263/what-is-the-maximum-allowed-limit-on-the-length-of-a-process-name
        pgrep = subprocess.run(["pgrep", program[:15]], stdout=subprocess.DEVNULL)
        return pgrep.returncode == 0


def start_program(program, args: list[str]):
    path = shutil.which(program)
    subprocess.Popen([path] + args, start_new_session=True)


def program_is_visible(workspace, window_class):
    class FoundIt(Exception):
        pass

    # Check if the program's workspace contains the focused window. This could be not
    # the case if for example the focused window is on a workspace on a different screen
    # that is displayed concurrently.
    def search(node):
        class_ = node.get("window_properties", {}).get("instance", "")
        is_focused = node.get("focused", False)
        if is_focused and class_.lower() == window_class.lower():
            raise FoundIt()

        if "nodes" in node:
            for child in node["nodes"]:
                search(child)
        if "floating_nodes" in node:
            for child in node["floating_nodes"]:
                search(child)

    try:
        search(workspace)

        return False
    except FoundIt:
        return True


def program_is_on_scratchpad(workspace):
    # The scratchpad has a workspace name of __i3_scratch
    return "scratch" in workspace.get("name", "")


def show_program(window_class, width=None, height=None):
    cmd = f'[class="(?i){window_class}"] scratchpad show; floating enable; '
    if width is not None:
        cmd += f"resize set width {width}px; "
    if height is not None:
        cmd += f"resize set height {height}px; "
    cmd += "move position center"
    subprocess.run([I3MSG_PATH, cmd])


def hide_program(window_class):
    cmd = f'[class="(?i){window_class}"] move scratchpad'
    subprocess.run([I3MSG_PATH, cmd])


def main():
    parser = ArgumentParser(description="Toggle a window to and from the scratchpad")
    parser.add_argument("--class", dest="window_class", help="WM_CLASS of the window")
    parser.add_argument("--width", type=int, help="Resize window to this width")
    parser.add_argument("--height", type=int, help="Resize window to this height")
    parser.add_argument("program", nargs="+", help="Program and options")
    args = parser.parse_args()

    program, *program_args = args.program
    window_class = args.window_class
    width, height = args.width, args.height

    if window_class is None:
        window_class = program.title()

    if program_runs(program):
        workspace = find_program_workspace(window_class)

        if program_is_visible(workspace, window_class):
            hide_program(window_class)
        elif program_is_on_scratchpad(workspace):
            show_program(window_class, width, height)
        else:
            # The program is visible on a workspace other than the active one so we move
            # it here via the scratchpad
            hide_program(window_class)
            show_program(window_class, width, height)
    else:
        start_program(program, program_args)


if __name__ == "__main__":
    main()
