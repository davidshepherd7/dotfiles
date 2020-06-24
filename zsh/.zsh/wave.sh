

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
