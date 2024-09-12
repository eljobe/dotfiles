# -*-sh-*-

# Find and files I don't mean to be leaving around.
# Only operates on the current directory recursively.
# Usually run it like:
#   $> clean
#      file1~
#      file2~
#      dir1/file3~
#
#   $> clean --delete
#
alias clean='find . -type f -name "*~" -not -path "*\.snapshot*"'

# Turn on auto color for common commands
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Aliases used to change the version of Java which is active.
# These should really only be added if sdkman is installed.
#   TODO(pepper): Make an sdkman module in the configs.
#
alias j7='sdk use java 7'
alias j8='sdk use java 8'
alias j9='sdk use java 9'

# Runs the gradlew wrapper script around Gradle with whatever
# additional arguments.
alias gw='./gradlew $@'

# Things to make working with go a little easier.
alias gta='go test ./...'
alias gtav='ga test -v ./...'

# Get my current external ipv4 address.
alias myip="curl --ipv4 icanhazip.com"

# Source alias files from auxiliarly sources.
for aliasfile in ${HOME}/.config/{,aux/}zsh/aliases.d/*aliases(N); do
    if [ -r $aliasfile ]; then
	source $aliasfile
    fi
done
unset aliasfile
