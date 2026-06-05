# Pi

An Emacs client for [Pi Coding Agent](https://pi.dev/)

## Setup

### Install the Pi Agent

```bash
npm install -g --ignore-scripts @earendil-works/pi-coding-agent

# Start Pi and run `/login` to configure your provider
pi
```

### Install `pi.el`

```elisp
(use-package pi
  :ensure t
  :vc (:url "git@github.com:ananthakumaran/pi.el.git"
       :rev :newest)
  :commands (pi-chat))
```

## Usage

`M-x pi-chat` from your project folder, this starts the Pi Chat
window. Enter your prompts to give instructions. To see the available
slash commands, type `/` in the prompt. You can also run Bash commands
using `!`, for example, `! echo 'hello'`. Use `!!` to execute the
command without adding it to the context.

## Sandbox

You can run Pi inside a sandbox by customizing `pi-executable` and `pi-flags`:

```elisp
(setq pi-executable "nono")
(setq pi-flags '("run" "--silent" "--profile" "pi" "--allow-cwd" "--" "pi" "--tools" "read,bash,edit,write,grep,find,ls"))
```
