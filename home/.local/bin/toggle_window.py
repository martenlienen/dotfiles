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

        if node.get("window_properties", {}).get("class") == window_class:
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
    pgrep = subprocess.run(["pgrep", program], stdout=subprocess.DEVNULL)
    return pgrep.returncode == 0


def start_program(program):
    path = shutil.which(program)
    subprocess.Popen([path], start_new_session=True)


def program_is_visible(workspace):
    class FoundIt(Exception):
        pass

    # Check if the program's workspace contains the focused window. This could be not the
    # case if for example the focused window is on a workspace on a different screen that
    # is displayed concurrently.
    def search(node):
        if node.get("focused") == True:
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
    subprocess.run([I3MSG_PATH, f'[class="{window_class}"] scratchpad show'])


def hide_program(window_class):
    subprocess.run([I3MSG_PATH, f'[class="{window_class}"] move scratchpad'])


def main():
    parser = ArgumentParser(description="Toggle a window to and from the scratchpad")
    parser.add_argument("--class", dest="window_class", help="WM_CLASS of the window")
    parser.add_argument("program")
    args = parser.parse_args()

    program = args.program
    window_class = args.window_class

    if window_class is None:
        window_class = program.title()

    if program_runs(program):
        workspace = find_program_workspace(window_class)

        if program_is_visible(workspace):
            hide_program(window_class)
        elif program_is_on_scratchpad(workspace):
            show_program(window_class)
        else:
            # The program is visible on a workspace other than the active one so we move
            # it here via the scratchpad
            hide_program(window_class)
            show_program(window_class)
    else:
        start_program(program)


if __name__ == "__main__":
    main()
