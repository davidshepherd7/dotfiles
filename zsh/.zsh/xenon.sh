#!/usr/bin/zsh -f

XENON_BIN_DIR="$HOME/code/boron-unstable/build/bin"

xenon-secret() {
    jq -r < ~/Xenon/api-token-parameters.json '.secret'
}

make-xenon-token() {
    local USER_ID=1
    "$XENON_BIN_DIR/auth-token" \
        --iss "pa_auth_api_token_handler" \
        --key "$(xenon-secret)" \
        --payload "{\"sub\": $USER_ID}"
}

xenon-auth-header() {
    echo "Authorization: Bearer $(make-xenon-token)"
}


# XENON_AUTH_TOKEN="$(make-xenon-token)"
# set-xenon-auth-token() {
#     XENON_AUTH_TOKEN="$(make-xenon-token)"
# }

xcurl() {
    curl -k -H "$(xenon-auth-header)" "$@"
}
