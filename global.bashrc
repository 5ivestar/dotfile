alias al='grep alias ~/dotfile/global.bashrc'
alias e='emacsclient -a ""'
alias ekill="emacsclient -e '(kill-emacs)'"
alias ec='emacsclient -a "" -c'

#for docker
alias dpa='echo "docker ps -a" && docker ps -a'
alias da='echo "docker attach" && docker attach'
alias dstart='echo "docker start" && docker start'
alias dstop='echo "docker stop" && docker stop'
alias dcp='echo "docker cp" && docker cp' 
alias drm='echo "docker rm" && docker rm'
alias dstoprm='echo "docker stop && docker rm"'
alias drmidang='docker images --filter "dangling=true" -q --no-trunc | xargs docker rmi'
alias drmdang='docker volume ls -f "dangling=true" -q | xargs docker volume rm'
alias dc='docker-compose'

function drestart(){
    echo "docker stop $1"
    docker stop $1
    echo "docker start $1"
    docker start $1
}

function de0_func(){
    echo "docker exec -it -u 0 -e COLUMNS=$COLUMNS -e LINES=$LINES -e TERM=$TERM $@ bash"
    docker exec -it -u 0 -e COLUMNS=$COLUMNS -e LINES=$LINES -e TERM=$TERM $@ bash
}
alias de0='de0_func'

function de_func(){
    echo "docker exec -it -e COLUMNS=$COLUMNS -e LINES=$LINES -e TERM=$TERM $@ bash"
    docker exec -it -e COLUMNS=$COLUMNS -e LINES=$LINES -e TERM=$TERM $@ bash
}
alias de='de_func'

function dstoprm(){
    echo "docker stop $1 && docker rm $1"
    docker stop $1 && docker rm $1
}

#for amaterasu
alias gradleclean='./gradlew clean && rm -rf ./gradle && ./gradlew --stop'


#for git
alias gd='echo "git diff" && git diff'
alias gdc='echo "git diff --cached" && git diff --cached'
alias gs='echo "git status" && git status'
alias gss='echo "git status -s" && git status -s'
alias gpo='echo "git push origin" && git push origin'
alias gcm='echo "git commit -m" && git commit -m'
alias ga='echo "git add" && git add'


function urlencode {
  echo "$1" | nkf -WwMQ | tr = %
}

alias urldecode='nkf -w --url-input'

alias ll='ls -alht'
alias csvline="python -c 'import sys;import csv; print(len(list(csv.reader(open(sys.argv[1])))))'"
