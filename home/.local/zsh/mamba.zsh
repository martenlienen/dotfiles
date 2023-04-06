# Modified output of $(mamba init)
__conda_setup="$('$MAMBA_ROOT/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "$MAMBA_ROOT/etc/profile.d/conda.sh" ]; then
        . "$MAMBA_ROOT/etc/profile.d/conda.sh"
    else
        export PATH="$MAMBA_ROOT/bin:$PATH"
    fi
fi
unset __conda_setup

if [ -f "$MAMBA_ROOT/etc/profile.d/mamba.sh" ]; then
    . "$MAMBA_ROOT/etc/profile.d/mamba.sh"
fi
