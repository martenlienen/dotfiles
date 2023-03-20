import textwrap


def import_if_available(module, alias=None, setup=None):
    if alias:
        import_ = "{} as {}".format(module, alias)
    else:
        import_ = module

    if setup is None:
        setup = ""
    else:
        setup = textwrap.indent(setup, "    ")

    return """
try:
    import {}
{}
except ImportError:
    pass
""".format(import_, setup).strip()


c = get_config()
c.InteractiveShellApp.extensions = ["autoreload"]
c.InteractiveShellApp.exec_lines = [
    "from pathlib import Path",
    import_if_available("numpy", "np", setup="rng = np.random.default_rng()"),
    import_if_available("pandas", "pd"),
    import_if_available("matplotlib.pyplot", "pp", setup="pp.style.use('marten')"),
    import_if_available("torch"),
    "%autoreload 3",
    "%xmode verbose",
]
c.TerminalInteractiveShell.confirm_exit = False

# Render figures in hiDPI
c.InlineBackend.figure_format = "retina"

# Do not tighten the bounding box
c.InlineBackend.print_figure_kwargs = {"bbox_inches": None}

# Don't overwrite any rc settings during interactive use
c.InlineBackend.rc = {}
