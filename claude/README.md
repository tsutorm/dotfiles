# Claude Code の設定

- `statusline.sh` — 2 行のステータスライン（モデル/effort/style、ディレクトリ/ブランチ/worktree、セッション識別子、ctx・5h・7d のバーと %、Cost）。表示のたびに使用量を `~/.claude/state/usage/<session_id>.json` に保存する。
- `bin/claude-usage` — 保存された使用量を読み、残量と推奨アクションを出す CLI。`--hook` でフックとして動き、閾値を超えたときだけ Claude の文脈に通知する。
- `settings.fragment.json` — `statusLine` と hooks（UserPromptSubmit / PostToolUse）の設定。
- `CLAUDE.usage.md` — 通知を受けた Claude の動き方。
- `CLAUDE.browser.md` — Web ブラウジングの道具の使い分け（WebFetch / agent-browser など）。

`CLAUDE.*.md` は `~/.claude/CLAUDE.md` に差し込まれる。先頭行と末尾行のマーカー（`<!-- 名前:begin -->` / `<!-- 名前:end -->`）の間だけが差し替わり、それ以外の内容はそのまま残る。新しい節はこの形式のファイルを足すだけで配れる。

## 導入

```bash
./claude/install.sh --dry-run   # 変更内容の確認
./claude/install.sh
```

依存: bash, jq, git。スクリプトはこのリポジトリへのシンボリックリンクになるので、編集はリポジトリ側で行う。
上書き前のファイルは `~/.claude/backups/` に退避される。

閾値は環境変数で変更できる（settings.json の `env` など）:
`CLAUDE_USAGE_CTX_WARN/CRIT`（既定 60/80）、`CLAUDE_USAGE_5H_WARN/CRIT`（70/90）、`CLAUDE_USAGE_7D_WARN/CRIT`（80/95）。
