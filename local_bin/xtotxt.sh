#!/usr/bin/env bash
# Download an X post's text through the official X API v2.

set -euo pipefail

usage() {
  printf 'Usage: %s <post-id-or-url> [output-file]\n' "${0##*/}"
  printf '\nRequires X_BEARER_TOKEN\n'
  printf 'Example: %s https://x.com/UserName/status/2086088408691060123\n' "${0##*/}"
}

die() {
  printf 'Error: %s\n' "$*" >&2
  exit 1
}

format_views() {
  local views=$1 divisor suffix whole decimal

  if ((views >= 1000000000)); then
    divisor=1000000000
    suffix=Md
  elif ((views >= 1000000)); then
    divisor=1000000
    suffix=M
  elif ((views >= 1000)); then
    divisor=1000
    suffix=k
  else
    printf '%s vues' "$views"
    return
  fi

  whole=$((views / divisor))
  decimal=$(((views % divisor) * 10 / divisor))
  if ((decimal == 0)); then
    printf '%s%s vues' "$whole" "$suffix"
  else
    printf '%s,%s%s vues' "$whole" "$decimal" "$suffix"
  fi
}

format_date() {
  date --date="$1" '+%I:%M %p · %d/%m/%Y' || die 'could not format post timestamp'
}

if (($# == 0)); then
  usage >&2
  exit 2
fi

if [[ $1 == --help || $1 == -h ]]; then
  usage
  exit 0
fi

if (($# > 2)); then
  usage >&2
  exit 2
fi

for command in curl date yq; do
  command -v "$command" >/dev/null 2>&1 || die "required command not found: $command"
done

[[ -n ${X_BEARER_TOKEN:-} ]] || die 'X_BEARER_TOKEN is not set'

input=$1
if [[ $input =~ ^[0-9]+$ ]]; then
  post_id=$input
elif [[ $input =~ ^https?://(www\.)?(x\.com|twitter\.com)/[^/]+/status/([0-9]+)([/?#].*)?$ ]]; then
  post_id=${BASH_REMATCH[3]}
else
  die 'provide a numeric post ID or an X status URL'
fi

output=${2:-"${post_id}.txt"}
response=$(mktemp)
trap 'rm -f "$response"' EXIT

status=$(curl -sSL -o "$response" --write-out '%{http_code}' \
  --header "Authorization: Bearer $X_BEARER_TOKEN" \
  "https://api.x.com/2/tweets/${post_id}?tweet.fields=note_tweet,created_at,public_metrics,author_id&expansions=author_id&user.fields=name,username") || die 'request to X API failed'

if [[ $status != 200 ]]; then
  message=$(yq -r '(.errors[0].detail // .detail // .title // .message // "X API request failed")' "$response" 2>/dev/null || true)
  die "X API returned HTTP $status: $message"
fi

text=$(yq -r '.data.note_tweet.text // .data.text // ""' "$response") || die 'could not parse X API response'
[[ -n $text ]] || die 'X API response did not contain post text'
username=$(yq -r '.data.author_id as $author_id | .includes.users[] | select(.id == $author_id) | .username' "$response") || die 'could not parse post author'
author_name=$(yq -r '.data.author_id as $author_id | .includes.users[] | select(.id == $author_id) | .name' "$response") || die 'could not parse post author name'
created_at=$(yq -r '.data.created_at // ""' "$response") || die 'could not parse post timestamp'
views=$(yq -r '.data.public_metrics.impression_count // ""' "$response") || die 'could not parse post views'
[[ -n $author_name && -n $username && -n $created_at && $views =~ ^[0-9]+$ ]] || die 'X API response did not contain required post metadata'

{
  printf 'By %s - @%s\n' "$author_name" "$username"
  printf 'https://x.com/%s/status/%s\n' "$username" "$post_id"
  printf '%s · %s\n-----\n\n' "$(format_date "$created_at")" "$(format_views "$views")"
  printf '%s\n' "$text"
} >"$output"
printf 'Wrote %s\n' "$output"
