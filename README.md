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

Run `M-x pi-chat` from any file in your project to start a Pi chat
session.

The chat buffer is read-only except for the prompt input area. Use
`RET` to submit a prompt and `C-j` to insert a newline. Press `i` from
anywhere in the chat buffer to move point to the prompt input area.

### Slash Commands

[Slash commands](https://pi.dev/docs/latest/usage#slash-commands) support completion in the prompt buffer. Type `/`
and press `C-M-i` (or any key bound to completion) to see available
commands.

### Bash

Prefix a command with `!` to run it in Bash. Use `!!` to run a command
without adding it to the conversation context.

```bash
! echo "hello"
```

### Sandbox

To run Pi inside a sandbox, customize `pi-executable` and `pi-flags`:

```elisp
(setq pi-executable "nono")
(setq pi-flags '("run" "--silent" "--profile" "pi" "--allow-cwd" "--" "pi" "--tools" "read,bash,edit,write,grep,find,ls"))
```

## How It Works

Emacs starts [Pi](https://pi.dev/docs/latest/rpc) in RPC mode. In this setup, Pi handles the agent
logic while Emacs provides the user interface.

Some features, such as `/login` and `/logout`, are not [supported](https://github.com/earendil-works/pi/issues/885)
because they are not currently exposed through the RPC API.

## Keybindings

### Chat Buffer Keybindings

| Key          | Command                    | Description                                                                          |
|--------------|----------------------------|--------------------------------------------------------------------------------------|
| `RET`        | `pi-visit-item`            | Jump to the source location at point; with a prefix argument, open in another window |
| `C-g`        | `pi-abort`                 | Abort the current operation                                                          |
| `TAB`, `C-i` | `pi-toggle-section`        | Toggle visibility of the section at point                                            |
| `n`, `M-n`   | `pi-goto-next-section`     | Move to the next section                                                             |
| `p`, `M-p`   | `pi-goto-previous-section` | Move to the previous section                                                         |
| `l`, `M-g l` | `pi-goto-last-section`     | Jump to the most recent section                                                      |
| `i`          | `pi-focus-prompt`          | Focus the prompt input field                                                         |
| `q`          | `pi-quit-chat`             | Quit the chat buffer                                                                 |

### Prompt Input Keybindings

| Key     | Command                    | Description                                            |
|---------|----------------------------|--------------------------------------------------------|
| `C-g`   | `pi-abort`                 | Abort the current operation                            |
| `M-p`   | `pi-previous-prompt`       | Recall the previous prompt from history                |
| `M-n`   | `pi-next-prompt`           | Recall the next prompt from history                    |
| `C-r`   | `pi-search-prompt`         | Search prompt history                                  |
| `RET`   | `widget-field-activate`    | Send the current prompt                                |
| `M-RET` | `pi-send-prompt-alternate` | Send the prompt using the alternate streaming behavior |
| `M-g l` | `pi-goto-last-section`     | Jump to the most recent section                        |


## Custom Variables

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
 ("set-auto-compaction" pi-set-auto-compaction 0)
 ("set-auto-retry" pi-set-auto-retry 0) ("session" pi-session-stats 0)
 ("name" pi-set-session-name 1)
 ("set-thinking-level" pi-set-thinking-level 0)
 ("cycle-model" pi-cycle-model 0)
 ("cycle-thinking-level" pi-cycle-thinking-level 0)
 ("set-steering-mode" pi-set-steering-mode 0)
 ("set-follow-up-mode" pi-set-follow-up-mode 0) ("fork" pi-fork 0)
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

#### pi-visit-tool-result-functions

<details><summary>Default Value</summary>

```elisp
(("read" . pi-visit-read-result) ("write" . pi-visit-write-result)
 ("edit" . pi-visit-edit-result) ("grep" . pi-visit-grep-result))
```

</details>

Alist mapping tool names to result visitor functions.

Each entry is (TOOL-NAME . FUNCTION) where FUNCTION is called
with (DETAILS ARGS) to visit the relevant location of the tool result.

#### pi-visit-tool-call-functions

<details><summary>Default Value</summary>

```elisp
(("read" . pi-visit-read-call) ("write" . pi-visit-write-call)
 ("edit" . pi-visit-edit-call))
```

</details>

Alist mapping tool names to call visitor functions.

Each entry is (TOOL-NAME . FUNCTION) where FUNCTION is called
with (ARGS) to visit the relevant location of the tool call.

