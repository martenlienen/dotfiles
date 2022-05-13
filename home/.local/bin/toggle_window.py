#!/usr/bin/env python3

import json
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
    # We match against the process name, so that we don't match toggle_window itself.
    # However, the process name is at most 16 null-terminated bytes [1], so we only
    # search for the first 15 characters.
    #
    # [1] https://stackoverflow.com/questions/23534263/what-is-the-maximum-allowed-limit-on-the-length-of-a-process-name
    pgrep = subprocess.run(["pgrep", program[:15]], stdout=subprocess.DEVNULL)
    return pgrep.returncode == 0


def start_program(program, args: list[str]):
    path = shutil.which(program)
    subprocess.Popen([path] + args, start_new_session=True)


def program_is_visible(workspace, program):
    class FoundIt(Exception):
        pass

    # Check if the program's workspace contains the focused window. This could be not the
    # case if for example the focused window is on a workspace on a different screen that
    # is displayed concurrently.
    def search(node):
        instance = node.get("window_properties", {}).get("instance")
        is_focused = node.get("focused", False)
        if is_focused and instance == program:
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


def show_program(window_class):
    cmd = f'[class="(?i){window_class}"] scratchpad show; move position center'
    subprocess.run([I3MSG_PATH, cmd])


def hide_program(window_class):
    cmd = f'[class="(?i){window_class}"] move scratchpad'
    subprocess.run([I3MSG_PATH, cmd])


def main():
    parser = ArgumentParser(description="Toggle a window to and from the scratchpad")
    parser.add_argument("--class", dest="window_class", help="WM_CLASS of the window")
    parser.add_argument("program", nargs="+", help="Program and options")
    args = parser.parse_args()

    program, *program_args = args.program
    window_class = args.window_class

    if window_class is None:
        window_class = program.title()

    if program_runs(program):
        workspace = find_program_workspace(window_class)

        if program_is_visible(workspace, program):
            hide_program(window_class)
        elif program_is_on_scratchpad(workspace):
            show_program(window_class)
        else:
            # The program is visible on a workspace other than the active one so we move
            # it here via the scratchpad
            hide_program(window_class)
            show_program(window_class)
    else:
        start_program(program, program_args)


if __name__ == "__main__":
    main()
