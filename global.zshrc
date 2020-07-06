
### cd -<TAB> for directory history
DIRSTACKSIZE=100
setopt AUTO_PUSHD

autoload -Uz compinit && compinit

zstyle ':completion:*' menu select
zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle ':completion:*:descriptions' format '%BCompleting%b %U%d%u'

### rich Ctrl-R history search
function peco-history-selection() {
    if [ -z ${BUFFER} ];
    then
        Result=`history -n 1 | tac | awk '!a[$0]++' | peco --prompt "searching command history >"`
    else
        Result=`history -n 1 | tac | awk '!a[$0]++' | peco --query ${BUFFER} --prompt "searching command history >"`
    fi
    if [ ! -z $Result ];
    then
        BUFFER=$Result
    fi
    CURSOR=$#BUFFER
    zle reset-prompt
}

zle -N peco-history-selection
bindkey '^R' peco-history-selection
source ~/.bashrc

### rich Ctrl-T directory path search
function peco-file-path-selection() {
    declare -a cm=($(echo $BUFFER))
    reverse=$(echo $BUFFER|rev)
    peco_prompt="[$BUFFER] searching under current directory >"
    if [ -z $BUFFER ];then
        peco_prompt="[cd] searching under current directory >"
        filename=`find . -type d|peco --prompt ${peco_prompt}`
        command="cd $filename"
    elif [ ${#cm[@]} -eq 0 ];then
        filename=`find . |peco --prompt ${peco_prompt}`        
        command="$BUFFER $filename"
    else
        query=`echo $BUFFER | rev |cut -d " " -f1 |rev`
        if [ ${reverse:0:1} = " " ];then
            query=""
        else
            cm=(${cm[@]:0:((${#cm[@]}-1))})
        fi
        if [ ${reverse:0:1} = "/" ];then
            find_dir=$(eval "echo $query")
            peco_prompt="[$BUFFER] searching under ${query} >"
            peco_query=""
        else
            find_dir="."
            peco_query=$query
        fi

        arg1=${cm[1]}
        arg2=${cm[2]}
        if [ $arg1 = "cd" ];then
            peco_prompt="[cd] searching dirrectory under $find_dir >"
            peco_list="find ${find_dir} -type d"
        elif [ $arg1 = "git" ] && [ $arg2 = "checkout" ];then
            peco_prompt="[git checkout] searching git branch and updated files >"
            x='"$(git branch -a)"'
            y='"$(git diff --name-only)"'
            peco_list="echo $x\n$y"
        elif [ $arg1 = "git" ] && [ $arg2 = "add" ];then
            peco_prompt="[git add] searching updated files and untracked files >"
            x='"$(git status -s |cut -b 4-)"'
            peco_list="echo $x"
        elif [ $arg1 = "de" ] || [ $arg1 = "de0" ] || [ $arg1 = "dstop" ] || [ $arg1 = "dstart" ] || [ $arg1 = "drm" ] || [ $arg1 = "dstoprm" ];then
            peco_prompt="[$BUFFER] searching docker containers >"
            peco_list='docker ps -a --format "{{.Names}}"'
        elif [ $arg1 = "docker" ] && [ $arg2 = "run"  ];then
            peco_prompt="[$BUFFER] searching docker images"
            peco_list='docker images --format "{{.Repository}}:{{.Tag}}"'
        else # normal case
            peco_list="find ${find_dir}"
        fi
        ## running peco here
        if [ -z $peco_query ]; then
            filename="$peco_list  |peco --prompt \"${peco_prompt}\""
        else
            filename="$peco_list  |peco --query ${peco_query} --prompt \"${peco_prompt}\""
        fi
        echo $filename
        filename=$(eval $filename)
        cm+=( $filename )
        command=$(printf " %s" "${cm[@]}")
        command=${command:1}
    fi
    if [ ! -z $filename ];
    then
        BUFFER=$command
    fi
    CURSOR=$#BUFFER
    zle reset-prompt
}


zle -N peco-file-path-selection
bindkey '^T' peco-file-path-selection

function ggrep(){
    result=`git grep -n $1|peco`
    file_name=$(echo $result | cut -d ":" -f1)
    line_num=$(echo $result | cut -d ":" -f2)
    if [ ! -z $result ];then
        e +${line_num} $file_name
    fi
}

alias eg="ggrep"

function ep(){
    if [ ! -z $1 ];then
        result=`git ls-files | peco --query $1`
    else
        result=`git ls-files | peco`
    fi
    file_name=$(echo $result | cut -d ":" -f1)
    if [ ! -z $result ];then
        e $file_name
    fi
}
