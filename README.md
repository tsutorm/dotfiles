# dotfiles

WSL2 上の Ubuntu 24.04 をクリーンインストールした状態から、開発環境を 1 コマンドで復帰する。

## 使い方

```bash
git clone https://github.com/tsutorm/dotfiles.git ~/git/hub/tsutorm/dotfiles
cd ~/git/hub/tsutorm/dotfiles
./bootstrap.sh
```

何度実行しても同じ結果になる。すでに入っているものは `ok` と表示して飛ばす。

| オプション | 内容 |
|---|---|
| `--dry-run` | 変更せず、何をするかだけ表示する |
| `--only home,runtime` | 指定したステップだけ実行する |
| `--skip apt` | 指定したステップを除いて実行する |
| `--with-rocm` | AMD GPU / ROCm も入れる（GPU のあるマシンのみ） |
| `RUNTIME_MANAGER=mise` | ランタイム管理に mise を使う（既定は asdf。一度選ぶと記録され、次回以降も使われる） |

## ステップ

| ステップ | 内容 |
|---|---|
| `apt` | `packages/apt.txt` と、Docker Engine・Google Cloud CLI の公式リポジトリ |
| `rocm` | `--with-rocm` のときだけ。amdgpu-install と ROCm（WSL なので DKMS は入れない） |
| `home` | `home/` 以下を `~` の同じ位置へリンクし、`~/.bashrc` に `~/.bashrc.d/*.sh` の読み込みを 1 行足す |
| `runtime` | asdf または mise を入れ、`~/.tool-versions` の全ツールをインストールする |
| `globals` | `packages/npm.txt` のグローバル npm パッケージと、agent-browser の Chrome |
| `claude` | Claude Code（公式インストーラ）と `claude/` の設定：ステータスライン、使用量モニタ、ブラウジングの使い分け（[claude/README.md](claude/README.md)） |
| `apm` | [APM](https://github.com/microsoft/apm) |
| `wsl` | `/etc/wsl.conf` で systemd を有効にする |

## 構成

```
bootstrap.sh        入口
lib/common.sh       link / ensure_line / as_root などの共通処理
steps/*.sh          各ステップ
home/               ~ にリンクするファイル（.bashrc.d/, .gitconfig, .config/git/ignore, .tool-versions）
packages/           apt.txt, npm.txt
claude/             Claude Code の設定
test/unit.sh        単体テスト（一時 HOME の中だけで実行。root・ネットワーク不要）
test/e2e.sh         まっさらな ubuntu:24.04 コンテナで bootstrap を最初から通す（Docker が必要）
```

### シェル設定

`~/.bashrc` は Ubuntu 標準のまま残し、追加の設定は `home/.bashrc.d/` に用途ごとに分ける。
apm などのインストーラは `~/.bashrc` に直接追記するので、それとぶつからないようにするため。

- `10-path.sh`: `~/.local/bin` と `~/bin` を PATH に入れる
- `20-runtime.sh`: asdf / mise の初期化
- `30-direnv.sh`: direnv
- `40-wsl.sh`: Windows 側 Chrome を BROWSER に使う、ROCm の設定
- `90-functions.sh`: 関数

### ランタイムのバージョン

バージョンは `home/.tool-versions` だけで管理する。asdf も mise もこのファイルを読むので、
管理ツールを切り替えても同じ版が入る。

```bash
RUNTIME_MANAGER=mise ./bootstrap.sh --only runtime,globals   # asdf → mise
RUNTIME_MANAGER=asdf ./bootstrap.sh --only runtime,globals   # mise → asdf
```

## 導入後の手動手順

秘密情報は dotfiles に入れない。bootstrap の後に、マシンごとに次を行う。

- `gh auth login`
- `gcloud auth login` と `gcloud auth application-default login`
- `claude` を起動してログインする
- Windows 側で `wsl --shutdown` してから開き直す（systemd と docker グループの反映）

## テスト

```bash
./test/unit.sh        # 数秒
./claude/test.sh      # 数秒
./test/e2e.sh         # 15〜30 分（Python と Ruby のビルドを含む）
```
