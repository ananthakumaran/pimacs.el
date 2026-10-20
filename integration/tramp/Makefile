.DEFAULT_GOAL := help

export PIMACS_TRAMP_PORT ?= 2222
COMPOSE := docker compose -f compose.yaml
SSH := sshpass -p pimacs ssh -F /dev/null \
	-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
	-o GlobalKnownHostsFile=/dev/null -o PubkeyAuthentication=no \
	-o PreferredAuthentications=password -o ConnectTimeout=5 \
	-p "$(PIMACS_TRAMP_PORT)" pimacs@127.0.0.1

.PHONY: help up down ssh logs check require-sshpass

help:
	@printf '%s\n' \
	  'up     Build and start the SSH/Pi server' \
	  'down   Remove the server and its disposable data' \
	  'ssh    Open a shell on the server (requires sshpass)' \
	  'logs   Show server logs' \
	  'check  Run remote Pi and read the remote-only file (requires sshpass)' \
	  '' \
	  'Disposable loopback-only server; username/password: pimacs/pimacs' \
	  'TRAMP: /ssh:pimacs@127.0.0.1#$(PIMACS_TRAMP_PORT):/srv/pimacs-tramp/project/'

up:
	$(COMPOSE) up --build --detach --wait --wait-timeout 60

down:
	$(COMPOSE) down --volumes --remove-orphans

ssh: require-sshpass
	$(SSH)

logs:
	$(COMPOSE) logs --no-color server

check: require-sshpass
	$(SSH) /opt/pimacs-test/bin/pi --version
	$(SSH) cat /srv/pimacs-tramp/project/remote-only.txt

require-sshpass:
	@command -v sshpass >/dev/null || { echo 'Install sshpass on the client for ssh/check.' >&2; exit 1; }
