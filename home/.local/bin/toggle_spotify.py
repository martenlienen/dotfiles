#!/usr/bin/env python3

import json
import os
import subprocess

SPOTIFY_PATH = "/usr/bin/spotify"
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


def find_spotify_workspace():
    def search(node, workspace=None):
        if node.get("type") == "workspace":
            workspace = node

        if node.get("window_properties", {}).get("class") == "Spotify":
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

        raise Exception("Spotify window not found in the layout tree")
    except WorkspaceException as e:
        return e.workspace


def spotify_runs():
    pgrep = subprocess.run(["pgrep", "spotify"], stdout=subprocess.DEVNULL)
    return pgrep.returncode == 0


def start_spotify():
    os.execl(SPOTIFY_PATH, SPOTIFY_PATH)


def spotify_is_visible(workspace):
    class FoundIt(Exception):
        pass

    # Check if spotify's workspace contains the focused window
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


def spotify_is_on_scratchpad(workspace):
    # The scratchpad has a workspace name of __i3_scratch
    return "scratch" in workspace.get("name", "")


def show_spotify():
    subprocess.run([I3MSG_PATH, '[class="Spotify"] scratchpad show'])


def hide_spotify():
    subprocess.run([I3MSG_PATH, '[class="Spotify"] move scratchpad'])


def main():
    if spotify_runs():
        spotify_workspace = find_spotify_workspace()

        if spotify_is_visible(spotify_workspace):
            hide_spotify()
        elif spotify_is_on_scratchpad(spotify_workspace):
            show_spotify()
        else:
            # Spotify is visible on a workspace other than the active one so we
            # move it here via the scratchpad
            hide_spotify()
            show_spotify()
    else:
        start_spotify()


if __name__ == "__main__":
    main()
