<!-- usage-monitor:begin -->
## 使用量モニタ（context / 5h / weekly / cost）

- ステータスライン（`~/.claude/statusline.sh`）が使用量を `~/.claude/state/usage/<session_id>.json` に保存している。
- `~/.claude/bin/claude-usage` で現セッションの状況と推奨アクションを確認できる（`--json` で機械可読、`--all` で他セッション一覧）。
- 閾値（ctx 60/80%, 5h 70/90%, 7d 80/95%）を超えるとフックが `[usage] level=...` を文脈に注入する。受け取ったら従うこと:
  - **長い作業の開始前**・サブエージェントを並列起動する前には `claude-usage` を確認し、計画の規模を決める。
  - ctx warn: 全文読み・長い出力を避け、調査はサブエージェントに委譲。ctx crit: 区切りで作業状態（済/未/次の一手）をファイルやメモリに退避し、ユーザーに `/compact` を提案。
  - 5h warn: サブエージェントを sonnet/haiku に寄せ、並列数を減らす。5h crit: 新規の重い作業を始めず、再開手順を書き出して止める（reset 時刻を伝える）。
  - 7d warn/crit: 投機的作業を削り、優先度をユーザーに確認。
<!-- usage-monitor:end -->
