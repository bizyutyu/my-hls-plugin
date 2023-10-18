# my-hls-plugin

[Haskell Language Server](https://github.com/haskell/haskell-language-server/)（以後HLS）の試作したプラグインを 公開する為だけのリポジトリ。
試作したのはHLSプラグインのなので、bizyutyuが追加・変更した部分は一部である（下記参照）。
帰納関数プログラミングシステム [MagicHaskeller](https://nautilus.cs.miyazaki-u.ac.jp/%7Eskata/MagicHaskeller.html/) を立てたサーバーと通信することで、入出力例から関数を [コードレンズ](https://learn.microsoft.com/en-us/visualstudio/ide/find-code-changes-and-other-history-with-codelens?view=vs-2022/) として提案する。MagicHaskellerが凄い。
git clone しても、サーバーIPとポート番号が設定されていない為、そのままビルドしても動かない。まさに公開用。

## bizyutyu が追加・変更した部分
- plugins/hls-magic-func-assistant-plugin/
- haskell-language-server.cabal
- cabal.project

## 関連ツールのバージョン
- 対象HLSバージョン : 1.9.0.0
- cabalバージョン : 3.6.2.0
- ghcバージョン : 8.10.7
