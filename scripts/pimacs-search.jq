def selected($kind): ($filters | index($kind)) != null;

def has_content_type($kind):
  (.message.content? // []) as $content
  | if ($content | type) == "array"
    then any($content[]; .type == $kind)
    else false
    end;


def text_content:
  if type == "array" then
    [.[] | select(.type == "text") | {type, text}]
  elif type == "string" then .
  else []
  end;

def assistant_content:
  if (.message.content | type) == "array" then
    [.message.content[]
     | select((.type == "text" and selected("assistant"))
              or (.type == "thinking" and selected("thinking"))
              or (.type == "toolCall" and selected("tool-call")))
     | if .type == "text" then {type, text}
       elif .type == "thinking" then {type, thinking}
       else {type, name, arguments}
       end]
  else .message.content
  end;

def projected:
  if .type == "message" then
    if .message.role == "user" then
      {type, message: {role: "user", content: (.message.content | text_content)}}
    elif .message.role == "assistant" then
      {type, message: {role: "assistant", content: assistant_content}}
    elif .message.role == "toolResult" then
      {type, message: {role: "toolResult", toolName: .message.toolName,
                       content: (.message.content | text_content)}}
    elif .message.role == "bashExecution" then
      {type, message: {role: "bashExecution", command: .message.command,
                       output: (.message.output | text_content)}}
    else {type}
    end
  elif .type == "compaction" then
    {type, summary, tokensBefore}
  else {type}
  end;
def accepted:
  if .type == "message" then
    if .message.role == "user" then selected("user")
    elif .message.role == "assistant" then
      (selected("assistant") and
       ((.message.content | type) == "string" or has_content_type("text"))) or
      (selected("thinking") and has_content_type("thinking")) or
      (selected("tool-call") and has_content_type("toolCall"))
    elif .message.role == "toolResult" then selected("tool-result")
    elif .message.role == "bashExecution" then selected("bash")
    else false
    end
  elif .type == "compaction" then selected("compact")
  else false
  end;

select(.type == "match")
| .data as $match
| ($match.lines.text | fromjson) as $entry
| if $entry.type == "session" then
    {kind: "session", path: $match.path.text, id: $entry.id, cwd: $entry.cwd, timestamp: $entry.timestamp}
  elif $entry.type == "session_info" then
    {kind: "session-info", path: $match.path.text, name: $entry.name}
  elif ($entry | accepted) then
    {kind: "entry", path: $match.path.text, offset: $match.absolute_offset, entry: ($entry | projected)}
  else
    empty
  end
