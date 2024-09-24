#!/usr/bin/env bash

set -euo pipefail

_usage() {
    cat <<HERE

$(basename "$0") VSN TARGET

Get OTP sources for version VSN from github, build, and install to TARGET.

HERE
    exit 0
}

_err() {
    echo "${1:-}"
    exit 1
}

_brew_install() {
    echo ok
}

_apt_install() {
    mapfile -td" " ps < <(echo -n "$*")
    local pps=()
    for p in "${ps[@]}"
    do echo -n "$p..."
       if 2>/dev/null apt -qq list "$p" | grep -qw installed
       then echo "ok."
       else echo "missing."
            pps+=("$p")
       fi
    done
    if [[ "${#pps[@]}" = 0 ]]
    then echo "deps ok."
    elif sudo true
    then sudo apt-get update
         sudo apt-get install -yq --no-install-recommends "$p"
    else _err "You need to install ${pps[*]}, with sudo."
    fi
}

_dep_install() {
    case $(uname -v) in
        *Darwin*) _brew_install "$*";;
        *Ubuntu*) _apt_install "$*";;
        *) _err "Unsuppported system."
    esac
}

_deps() {
    echo "Checking deps..."
    _dep_install \
        build-essential \
        ca-certificates \
        libncurses-dev \
        libpcap-dev \
        libsctp-dev \
        libsctp1 \
        libssl-dev \
        lksctp-tools \
        make
}

_newest() {
    local site=${1:?}
    local orgproj=${2:?}
    local vsn=${3:?}
    local re2="OTP-${vsn}[^\"]*"
    local re1="<a href=\"/$orgproj/releases/tag/$re2\""
    if tag="$(curl -sL "$site/$orgproj/releases" | grep -Eo "$re1" | grep -Eo "$re2" | head -n1)"
    then echo "$tag"
    else _err "not found at $site."
    fi
}

_http_get() {
    local url=${1:?}
    local dest=${2:?}
    1>&2 echo -n "getting: $url -> $dest..."
    if [ -f "$dest" ]
    then 1>&2 echo "cached."
         echo "$dest"
    elif mkdir -p "$(dirname "$dest")" &&
            curl -sSL "$url" -o "$dest"
    then 1>&2 echo "ok."
         echo "$dest"
    else _err "Download failed."
    fi
}

_untar() {
    local tgz=${1:?}
    local tag=${2:?}
    local dir

    dir=$(dirname "$tgz")
    tar -xzf "$tgz" -C "$dir"
    cd "$dir/otp-$tag"
    pwd
}

_configure() {
    local dir=${1:?}
    local dest=${2:?}

    case $(uname -s) in
        Darwin) sctp="--disable-sctp";;
        Linux) sctp="--enable-sctp=lib";;
    esac
    cd "$dir"
    2>/dev/null ./configure \
         "$sctp" \
         --prefix="$dest" \
         --without-debugger \
         --without-eldap \
         --without-erl_docgen \
         --without-et \
         --without-ftp \
         --without-hipe \
         --without-javac \
         --without-jinterface \
         --without-megaco \
         --without-observer \
         --without-odbc \
         --without-tftp \
         --without-wx \
         --without-dynamic-trace \
         --disable-lock-counter
}

_compile() {
    local dir=${1:?}
    cd "$dir"
    2>/dev/null \
        make -j8
}

_install() {
    local dir=${1:?}
    cd "$dir"
    2>/dev/null \
        make install
}

# our parameters
site="https://github.com"
org="erlang"
proj="otp"
vsn="${1:-}"
dest="${2:-}"

# check that we have mandatory args
[[ -n "$vsn" && -n "$dest" ]] || _usage

# if any of these stanzas fail, we have an error string in $res
res=$(_deps) &&
    echo "deps: $res" &&
    res=$(_newest $site "$org/$proj" "$vsn") &&
    tag=$res &&
    echo "tag: $tag" &&
    url="$site/$org/$proj/archive/refs/tags/$tag.tar.gz" &&
    res=$(_http_get "$url" "$dest/$tag.tgz") &&
    tgz=$res &&
    echo "get: $tgz" &&
    res=$(_untar "$tgz" "$tag") &&
    bdir=$res &&
    echo "untar: $bdir" &&
    tag=$(grep -Eo "[0-9.]+$" <<<"$tag") &&
    prefix="$dest/erl-$tag" &&
    res=$(_configure "$bdir" "$prefix") &&
    echo "Configured with prefix $prefix" &&
    res=$(_compile "$bdir") &&
    echo "Compiled." &&
    res=$(_install "$bdir") &&
    echo "Installed in $prefix." &&
    rm -rf "$tgz" &&
    rm -rf "$bdir" &&
    exit 0

# we only get here is there is a failure above
echo "$res"
exit 33
