#! /bin/zsh


export boron_dir="$HOME/code/boron-unstable"

bgulpl_once()
{
    # subshell to avoid changing directory
    (
        cd ~/code/boron-unstable/boron/web_applications/
        gulp --silent --reporter=simple --deploy_root ~/gulp_dest "$@"
    )
}
alias bgulp_once='bgulpl_once --nolint'
alias bgulpr_once="bgulp_once --done_command 'refresh-browser.sh'"


bgulpl()
{
    (
        cd ~/code/boron-unstable/boron/web_applications/
        gulp watch --done --silent --reporter=simple --deploy_root ~/gulp_dest "$@"
    )
}
alias bgulp="bgulpl --nolint"
alias bgulpr="bgulp --done_command 'refresh-browser.sh'"

alias lbiosite='$boron_dir/scripts/launch-biosite-single-terminal.sh -w ~/gulp_dest/app/'
alias lbiosite_client='$boron_dir/scripts/launch-biosite-single-terminal.sh -w ~/gulp_dest/app/ -c'

aburl()
{
    burl "$1" \
         -H "Authorization: Bearer $BORON_API_TOKEN" \
         "${@:2}"
}

acurl() {
    curl "$1" \
         -k \
         -H "Authorization: Bearer $BORON_API_TOKEN" \
         "${@:2}"
}

burl()
{
    if [[ "$#" < 1 ]] || [[ "$1" = "-h" ]]; then
        echo "Usage: bcurl [boron_api_server_path] [curl opts]" 1>&2
        return 1
    fi

    curl \
        -k  \
        "https://localhost:8088${1}" \
        "${@:2}"
}


picurl() {
    curl -k "https://192.168.0.250:8100${1}" -H "Authorization: Bearer $BORON_PI_API_TOKEN" "${@:2}"
}

dcurl() {
    curl "https://api.digitalocean.com/v2${1}" \
         -H "Authorization: Bearer $SOFTWARE_DO_API_TOKEN" \
         -H "Content-Type: application/json" \
         "${@:2}"
}

alias boron_client='$boron_dir/build/bin/boron-client'

cd-boron-build () {
    cd "$boron_dir/build"
}

boron-test () {
    local binary="$1"; shift
    (
        cd-boron-build && ninja "$binary" && "./bin/$binary" "$@"
    )
}
_boron-test () {
    reply=(`cd-boron-build && ninja -t targets | awk -F: '{print $1}' | grep 'test-\|test_' | grep -v '/CMakeLists.txt'`);
}
compctl -K _boron-test boron-test
alias esfix="eslint -c ~/code/boron-unstable/boron/web_applications/future-eslintrc.json --fix"

boron-cmake() {
    (cd "$boron_dir/build" && cmake .)
}

#
ssh-psql () {
    ssh "$@" "bash -c 'psql -U boron_user boron -h localhost'"
}
_ssh-psql () {
    local service=ssh
    _ssh "$@"
}
compdef _ssh-psql ssh-psql



boron-js () {
    hg manifest |\
        grep '\.js$' |\
        grep -v '/dev-app/\|/app/\|/lib/\|/lib-managed/' |\
        awk "{print \"$(hg root)/\" \$0}"
}

windows_boron_dir="/mnt/windows-boron"
pull-to-windows-boron () {
    (
        set -o errexit
        set -o nounset

        cd "$windows_boron_dir/common"
        hg pull "$boron_dir/common"
        hg strip -r 'not ancestors(tip)' || true

        cd ../
        hg pull "$boron_dir"
        hg up -r tip -C
        hg strip -r 'not ancestors(tip)' || true

        cd "$windows_boron_dir/common"
        hg up -r tip -C
    )
}

pull-from-windows-boron () {
    (
        cd "$boron_dir/common"
        hg pull "$windows_boron_dir/common"
        cd ../
        hg pull "$windows_boron_dir"
    )
}

replace-db () {
    "$boron_dir/scripts/replace-boron-db.sh" "$@" -ut -P ~/data/postcode_positions.csv
}

send-deb-to-iris-servers() {
    local version="$1"

    sudo cp "/redist/experimental/boron/installers/$version" "$HOME/$version" -r
    sudo chown -R david:david "$HOME/$version"
    scp "$HOME/$version/boron_$version.deb" iris-boron:~/
    scp "$HOME/$version/boron_$version.deb" iris-postgres:~/
}


copy-experimental-release() {
    if [[ "$#" -lt 1 ]]; then
        echo "Usage: $0 [version]" 1>&2
        return 1
    fi

    local version="$1"

    mkdir -p ~/boron-releases
    sudo mount /redist || true

    # Get the complete and master versions for both Ubuntu versions
    sudo cp "/redist/experimental/boron/installers/${version}/"boron{,_master_server}"_${version}_"ubuntu_*.deb ~/boron-releases
    sudo chown -R david:david ~/boron-releases

    # In case the nas goes down
    sudo umount /redist
}

alias boron-tags="ctags-exuberant -e -R boron/api_server boron/biometric_node boron/client boron/cscs_server boron/event_server boron/fs80_server boron/job_server boron/report_server boron/setup boron/shared boron/tests boron/tools"


firepi-ssh() {
    sshpass -p "$PI_PASSWORD" ssh pi@mercurypi.pa
}


uburl() {
    burl "$1" \
         -H "Authorization: Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJ0ZXN0X3VuYXV0aG9yaXNlZF91c2VyIn0.i8iGLQzs0myh2Bh-WYR4VRPQOelmUTgQ2fHJOLhiRqU" \
         "${@:2}"
}

boron-files() {
    find -name '*.js' -o -name '*.html' -o -name '*.cpp' -o -name '*.h' |\
        grep -v '\.spec\.js\|./web_applications/lib/\|./web_applications/node_modules/\|./web_applications/app/\|./web_applications/dev-app/\|./web_applications/lib-managed/\|/tests/'
}
