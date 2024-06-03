

alias wsql="~/code/monorepo/money-srv/bin/runin local sql"

alias wl="cd ~/code/monorepo && ~/code/monorepo/money-srv/bin/runin local"
alias wlt="cd ~/code/monorepo && ~/code/monorepo/money-srv/bin/runin local tool"

alias mm="make -C ~/code/monorepo"

alias recent-sms="watch -d -n 0.2 -x -- ~/code/monorepo/money-srv/bin/runin local sql -x -c \"SELECT when_created, their_number, body FROM text_messages WHERE when_created > NOW() at time zone 'utc' - interval '1 minutes' ORDER BY when_created DESC LIMIT 100\""

alias recent-ledger="watch -d -n 0.2 -x -- ~/code/monorepo/money-srv/bin/runin local sql -x -c \"SELECT ledger.ledger_entries.when_entered, ledger.ledger_accounts.description, mode, amount FROM ledger.ledger_entries JOIN ledger.ledger_accounts USING(ledger_account_id) WHERE when_entered > NOW() at time zone 'utc' - interval '2 minutes' ORDER BY when_entered DESC LIMIT 100\""

# alias all-bills-tests="cd ~/code/monorepo && make m.typecheck && cd money-srv && ./bin/run_tests unittests/logic/test_bills.py unittests/external unittests/interface/graphql/test_resolvers.py unittests/logic/test_bill_types.py -vv"
alias all-bills-tests="cd ~/code/monorepo && make m.typecheck && cd money-srv && ./bin/run_tests unittests/logic/test_bills.py unittests/external unittests/logic/test_bill_types.py unittests/logic/test_bill_partners.py -vv"


alias psql-prod-tunnel-up="gcloud compute ssh mm-db-followers-9j0t --zone europe-west1-c --project wavemm-174408 -- -L 55432:localhost:5432  -o ExitOnForwardFailure=yes -N -f -MS '/tmp/runin-ssh.{pid}:%r@%h'"

alias psql-prod-tunnel-down="gcloud compute ssh mm-db-followers-9j0t -- -S '/tmp/runin-ssh.{pid}:%r@%h' -O exit"


alias test-kubectl="kubectl --context=gke_david-k8s-experiments_europe-west1-b_test-cluster"


alias check-migration-cycle="mm m.migrate && ~/code/monorepo/money-srv/bin/runin local alembic downgrade -1 && ~/code/monorepo/money-srv/bin/runin local alembic upgrade +1 && ~/code/monorepo/money-srv/bin/runin local alembic downgrade -1 && ~/code/monorepo/money-srv/bin/runin local alembic upgrade +1"


android_emulator() {
    ~/Android/Sdk/tools/emulator -avd "Pixel_3a_API_30_x86" || true
}

android_clean() {
    adb shell pm clear com.wave.personal.dbg
    adb shell pm clear com.sendwave.agent.dbg
}

alias android_activity_stack="adb shell dumpsys activity activities |  sed -En -e '/Stack #/p' -e '/Running activities/,/Run #0/p'"

alias kubectl-mm="kubectl --context mm-ng"

mmlogs() {
    cd ~/code/monorepo
    unbuffer docker-compose logs --tail 100 -tf web worker amqp_events | grep -v 'Bad Network Fallback command' | sed -E -e 's/ #[a-zA-Z0-9_]+=.*//g'
}

# Get kotlin tools on my path
PATH="$PATH:/opt/android-studio/plugins/Kotlin/kotlinc/bin/"


alias toolsn="~/code/monorepo/money-srv/bin/runin local tool sn"
alias prodrw-toolsn="~/code/monorepo/money-srv/bin/runin prodrw tool sn"

kube_secret(){
    # Fuzzy matches should be possible, but I can't get xargs to work
    EDITOR="cat" kubectl --context prod edit secrets credentials \
        | grep "${1}:" \
        | sed -e 's/.*: //' \
        | base64 -d
    echo ""
}

# Playing with semgrep in typecheck
export WAVE_EXPERIMENTAL_SEMGREP_IN_TYPECHECK=1

# Make semgrep output in Emacs-compatible format
export WAVE_SEMGREP_REPORTING_STYLE="emacs"

alias coverage_any_version="firefox ~/code/monorepo/money-srv/.money-srv-venv/coverage/*/htmlcov/index.html"

# Broken linter
export BYPASS_RUN_SWIFTLINT=1
