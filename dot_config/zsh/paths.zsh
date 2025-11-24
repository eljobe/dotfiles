# -*-sh-*-

# This file is sourced both at the end of my ~/.zshenv and at the top
# of my .zshrc.
#
# The objective of these commands is to take whatever the system-wide
# definition of the path is, prepend and append my custom path
# elements. However, especially in the case of the .zshrc, what is
# actually happening is a reshuffling of the order of my custom path
# elements to the front or back of the path. I have to do this
# because on OS X's system-wide /etc/zprofile it prepends all of the
# paths from /etc/paths{.d} to whatever I've previously set up in my
# ~/.zshenv

# Note: The order in which paths are prepended and appended is
# important.

# Define a function to move a path to the end or simply append the
# path if it isn't already in the path array.
append_path() {
    local dir="$1"
    # Remove the path from 'path' array
    path=(${path:#$dir})
    # Append the path to the end
    path+=("$dir")
}

# Define a function to prepend the path. This is trival, but wrappeed
# in a function to make it more symmetric to appending.
prepend_path() {
    local dir="$1"
    # Since path is typeset -U (unique array) only the first instance
    # of $dir will survive.
    path=($dir $path)
}

# Make path a unique array.
# This ensures that any value only appears once in the array.
typeset -U path

# Homebrew locations vary a lot on my machines.
# Note, it is possible on a machine that both /usr/local and
# /opt/homebrew will exist. In that case, I want executables in
# /opt/homebrew to take precedence.
brewroots=('/usr/local' '/opt/homebrew' '/home/linuxbrew/.linuxbrew')
for root in $brewroots; do
    # If the homebrew root exists, add both sbin and bin to the path
    # whether or not they exist as subdirectories. This is because
    # brew doctor wants both of them to be on the path in case a new
    # sbin binary gets installed in the current session.
    if [ -d "$root" ]; then
				# sbin before bin so that bin takes relative precedence.
				prepend_path $root/sbin
				prepend_path $root/bin
    fi
done

for root in $brewroots; do
    # The llvm bottle in homebrew is where important compilation tools
    # live. Especially clang. Apple's clang is bad at compiling one
    # one of my work projects.
    if [ -d "$root/opt/llvm/bin" ]; then
				prepend_path "$root/opt/llvm/bin"
    fi
    # The QuickTime5 from homebrew needs to override the one from the
    # system.
    if [ -d "$root/opt/qt@5/bin" ]; then
				prepend_path "$root/opt/qt@5/bin"
    fi
    # The OpenJDK from homebrew needs to override the one from the
    # system.
    if [ -d "$root/opt/openjdk/bin" ]; then
				prepend_path "$root/opt/openjdk/bin"
    fi
		# The ruby binary from homebrew needs to override the one from
		# the system.
		if [ -d "$root/opt/ruby/bin" ]; then
				prepend_path "$root/opt/ruby/bin"
		fi
		# To find cocoapods, that gems bin directory is needed.
		if [ -d "$root/lib/ruby/gems/3.4.0/gems/cocoapods-1.16.2/bin" ]; then
				prepend_path "$root/lib/ruby/gems/3.4.0/gems/cocoapods-1.16.2/bin"
		fi
done

# Cargo is rust's wrapper and package manager.
if [ -f "$HOME/.cargo/env" ]; then
    prepend_path $HOME/.cargo/bin
fi

# Foundry is a bunch of tools for use with ethereum development.
foundrydir="$XDG_CONFIG_HOME/.foundry"
if [ -d "$foundrydir" ]; then
		prepend_path $foundrydir/bin
fi
unset foundrydir

# pnmp for managing npm packages
pnpmdir="$HOME/Library/pnpm"
if [ -d "$pnpmdir" ]; then
		prepend_path $pnpmdir
fi

# uv python package manager
uvdir="$HOME/.local/bin"
if [ -d "$uvdir" ]; then
		prepend_path $uvdir
fi

# On some linux systems go is in /usr/local/go/bin
godir="/usr/local/go/bin"
if [ -d "$godir" ]; then
		prepend_path $godir
fi

# Append my $HOME/bin directory.
append_path $HOME/bin

export PATH
unset -f append_path
unset -f prepend_path
