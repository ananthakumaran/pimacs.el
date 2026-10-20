FROM node:22.22.2-bookworm-slim

RUN apt-get update \
    && apt-get install --yes --no-install-recommends openssh-server jq ripgrep \
    && rm -rf /var/lib/apt/lists/* \
    && rm -f /etc/ssh/ssh_host_* \
    && npm install --global --ignore-scripts --prefix /opt/pimacs-test \
       "@earendil-works/pi-coding-agent" \
    && npm cache clean --force \
    && useradd --create-home --shell /bin/bash pimacs \
    && printf 'pimacs:pimacs\n' | chpasswd \
    && install -d /run/sshd \
    && install -d -o pimacs -g pimacs /srv/pimacs-tramp/project \
    && printf '%s\n' 'This file exists only on the TRAMP test server.' \
       > /srv/pimacs-tramp/project/remote-only.txt \
    && chown pimacs:pimacs /srv/pimacs-tramp/project/remote-only.txt

RUN printf '%s\n' \
    'HostKey /etc/ssh/ssh_host_ed25519_key' \
    'PidFile /run/sshd.pid' \
    'PubkeyAuthentication no' \
    'AllowUsers pimacs' \
    'PermitRootLogin no' \
    'PasswordAuthentication yes' \
    'PermitEmptyPasswords no' \
    'KbdInteractiveAuthentication no' \
    'UsePAM no' \
    'AllowAgentForwarding no' \
    'AllowTcpForwarding no' \
    'X11Forwarding no' \
    'Subsystem sftp internal-sftp' \
    > /etc/ssh/sshd_config

COPY fixture/package.json fixture/package-lock.json /srv/pimacs-tramp/fixture/
RUN cd /srv/pimacs-tramp/fixture && npm ci --ignore-scripts && npm cache clean --force
COPY fixture/src /srv/pimacs-tramp/fixture/src
COPY fixture/tapes /srv/pimacs-tramp/fixture/tapes
COPY project/.project /srv/pimacs-tramp/project/.project
COPY --chown=pimacs:pimacs project/agent/settings.json /srv/pimacs-tramp/project/agent/settings.json

WORKDIR /srv/pimacs-tramp/project

CMD ["sh", "-ec", "ssh-keygen -A; exec /usr/sbin/sshd -D -e"]
