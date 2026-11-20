# Sourced because ghostel-resurrect.el pointed ZDOTDIR here, in place of
# ghostel's shell-integration bootstrap.  Gives this terminal its own
# command history file, without any ~/.zshrc code.

# Hand ZDOTDIR back to ghostel's bootstrap and run its .zshenv, as zsh
# would have.  That restores the user's ZDOTDIR, sources ~/.zshenv and
# loads ghostel's shell integration; zsh then reads ~/.zshrc as usual.
'builtin' 'export' ZDOTDIR="$GHOSTEL_RESURRECT_ZDOTDIR"
'builtin' 'unset' 'GHOSTEL_RESURRECT_ZDOTDIR'
[[ ! -r "$ZDOTDIR/.zshenv" ]] || 'builtin' 'source' '--' "$ZDOTDIR/.zshenv"

# At the first prompt, after ~/.zshrc (and /etc/zshrc) have set HISTFILE
# and loaded that history, switch to this terminal's own history: `fc -p'
# sets HISTFILE to it and starts a fresh list read from it, keeping
# HISTSIZE, SAVEHIST and the history options.  New commands are then
# saved to it the way they would have been to the old one.
if [[ -o interactive && -n "$GHOSTEL_RESURRECT_HISTFILE" ]]; then
    'builtin' 'typeset' -g __ghostel_resurrect_histfile="$GHOSTEL_RESURRECT_HISTFILE"
    __ghostel_resurrect_history() {
        precmd_functions=(${precmd_functions:#__ghostel_resurrect_history})
        'builtin' 'fc' -p -- "$__ghostel_resurrect_histfile"
        'builtin' 'unset' '__ghostel_resurrect_histfile'
        'builtin' 'unfunction' '__ghostel_resurrect_history'
    }
    'builtin' 'typeset' -ga precmd_functions
    precmd_functions+=(__ghostel_resurrect_history)
fi
# Shells started from this one keep the usual history.
'builtin' 'unset' 'GHOSTEL_RESURRECT_HISTFILE'
