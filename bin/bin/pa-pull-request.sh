#!/bin/bash

set -o errexit
set -o nounset

sensible-browser "$(hg paths review)/pull-request/new" &

subject="PR+Boron%3A+"
to="software%40biositesystems.com"
body=""

gmail_biosite_account="0"


mail_url="https://mail.google.com/mail/u/${gmail_biosite_account}/?view=cm&to=$to&su=$subject&body=$body"
sensible-browser "$mail_url" &
