<!-- browser-rules:begin -->
## Web ブラウジングの使い分け

- **読むだけ（公開ページ・ログイン不要・操作不要）**: WebFetch / WebSearch を使う。ブラウザは起動しない。
- **操作が要る（クリック・フォーム入力・ログイン後の画面・JS で描画される SPA）**: `agent-browser` を使う。
  - 使う前に `agent-browser skills get core` で使い方を確認する（本体と同じバージョンの説明。`--help` から推測しない）。
  - 作業ごとに名前付きセッションを使う: `export AGENT_BROWSER_SESSION="$(agent-browser session id --scope worktree --prefix task)"`。既定のセッションは他のエージェントと共有されている。終わったら `agent-browser close` する。
  - 基本の流れ: `open <url>` → `snapshot -i`（@e1 などの参照を得る）→ `click @e1` / `fill @e2 "text"` → ページが変わったら再度 `snapshot -i`。
  - 描画後の本文だけ欲しいときは `agent-browser read`（開いているタブ）や `agent-browser read <url>` を使う。
  - 認証情報はコマンドラインに書かない。auth vault（`agent-browser auth save/login`）を使い、パスワードや CAPTCHA の入力はユーザーに任せる。
- **使わないもの**: Claude in Chrome（`--chrome`）は WSL 非対応なので WSL では使わない（Windows ネイティブの Claude Code でのみ検討）。
- **提案に留めるもの**: 性能計測・ネットワーク・コンソールの調査が必要なら、Chrome DevTools MCP（未導入）の導入をユーザーに提案する。
<!-- browser-rules:end -->
