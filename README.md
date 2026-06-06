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

### Custom Variables

#### pi-sync-request-timeout `2`

The number of seconds to wait for a sync response.

#### pi-executable `"pi"`

Pi command executable name.

#### pi-process-environment `nil`

List of extra environment variables to use when starting pi.

#### pi-flags `nil`

List of additional flags to provide when starting pi.

#### pi-log-rpc `nil`

When non-nil, log all RPC JSON to `pi-log-rpc-file`.

#### pi-log-rpc-file `"/tmp/pi.el.log"`

File to write RPC JSON log entries to.

#### pi-file-completion-backend `project`

Completion backend for @-prefixed file paths in prompts.
`project` uses `project-files` to list files in the current project.
`file` uses `file-name-all-completions` to list files under the project root.

#### pi-prompt-history-max-size `500`

Maximum number of prompt history entries to keep.

#### pi-resume-max-sessions `100`

Maximum number of recent sessions to list when resuming a session.

#### pi-prompt-streaming-behavior `followUp`

Default streaming behavior for prompts.

`steer`: Queue the message while the agent is running.  It is delivered
after the current assistant turn finishes executing its tool calls,
before the next LLM call.

`followUp`: Wait until the agent finishes.  Message is delivered only
when agent stops.

#### pi-slash-commands

<details><summary>Default Value</summary>

```elisp
(("model" pi-select-model 0) ("new" pi-new-session 0)
 ("resume" pi-resume 0) ("compact" pi-compact 1)
 ("session" pi-session-stats 0) ("name" pi-set-session-name 1)
 ("set-thinking-level" pi-set-thinking-level 0)
 ("cycle-model" pi-cycle-model 0) ("fork" pi-fork 0)
 ("clone" pi-clone 0) ("copy" pi-copy 0) ("export" pi-export 1)
 ("quit" pi-quit-chat 0) ("exit" pi-quit-chat 0))
```

</details>

Alist mapping slash command names to command specs.

Each entry is (NAME COMMAND MAX-ARGS) where NAME is the command
string without the leading slash, COMMAND is a command symbol,
and MAX-ARGS is 0 or 1 indicating the number of optional string
arguments the command accepts.

#### pi-insert-tool-args-functions

<details><summary>Default Value</summary>

```elisp
(("read" . pi-insert-read-args) ("write" . pi-insert-write-args)
 ("edit" . pi-insert-edit-args) ("bash" . pi-insert-bash-args)
 ("grep" . pi-insert-grep-args) ("find" . pi-insert-find-args)
 ("ls" . pi-insert-ls-args))
```

</details>

Alist mapping tool names to inserter functions.

Each entry is (TOOL-NAME . FUNCTION) where FUNCTION is called
with ARGS plist to insert formatted tool call arguments.

#### pi-insert-tool-result-functions

<details><summary>Default Value</summary>

```elisp
(("bash" . pi-insert-bash-result) ("read" . pi-insert-read-result)
 ("write" . pi-insert-write-result) ("edit" . pi-insert-edit-result)
 ("grep" . pi-insert-grep-result) ("find" . pi-insert-find-result)
 ("ls" . pi-insert-ls-result))
```

</details>

Alist mapping tool names to result inserter functions.

Each entry is (TOOL-NAME . FUNCTION) where FUNCTION is called
with (RESULT-TEXT DETAILS ARGS) to insert the tool execution result.

