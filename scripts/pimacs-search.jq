def selected($kind): ($filters | index($kind)) != null;

def has_content_type($kind):
  (.message.content? // []) as $content
  | if ($content | type) == "array"
    then any($content[]; .type == $kind)
    else false
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
| select($entry | accepted)
| {path: $match.path.text, offset: $match.absolute_offset, entry: $entry}
