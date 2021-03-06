これは何
========
[jaunte.el] をロクに説明も読まずにそこはかとなく移植。

  [jaunte.el]: http://kawaguchi.posterous.com/emacshit-a-hint

依存するライブラリ
==================
- [ansify](http://github.com/bowbow99/xyzzy.ansify/)
  - NetInstaller 用 [packages.l](http://bowbow99.sakura.ne.jp/xyzzy/packages.l)
- [ansi-loop](http://miyamuko.s56.xrea.com/xyzzy/ansi-loop/)
  - NetInstaller 用 [package.l](http://miyamuko.s56.xrea.com/xyzzy/package.l)

インストール
============

NetInstaller から
-----------------
[カフェイン中毒] からどうぞ。

  [カフェイン中毒]: http://bowbow99.sakura.ne.jp/xyzzy/packages.l

まにゅありー
------------
- [jaunte-0.01.00.zip] をダウンロード
- xyzzy フォルダに丸ごと解凍
- 設定へ進む


使い方
======
.xyzzy などに以下を書いておきます。

    (require "jaunte")
    (use-package :jaunte)
    
    (global-set-key '(#\C-c #\C-j) 'jaunte)

書いたら .xyzzy を読み込み直すなり xyzzy を再起動するなりして `C-c` `C-j` を押す
と、画面内のあちこちにヒントが表示されミニバッファで入力待ちになります。
ジャンプしたい位置にあるヒントを入力すると、カーソルがそこへ移動します。

設定
====

変数 `*jaunte-hint-target-keys*`
--------------------------------
ヒントに使う文字群。
この文字列に含まれる文字を順番にヒントとして使っていく。

※文字の種類が少ないとすぐヒント文字列が長くなってしまうので、ヒントを
表示するスペースが足りなくなってバグる。

変数 `*jaunte-inactive-hint-attribute*`
---------------------------------------
ヒットしてないヒントの文字色など

変数 `*jaunte-active-hint-attribute*`
-------------------------------------
ヒットしてるヒントの文字色など

変数 `*jaunte-keymap*`
-----------------------
jaunte 中に使用するキーマップ。

変数 `*jaunte-jump-if-unique*`
-------------------------------
絞り込んで残り1つになったら即ジャンプする。

変数 `*jaunte-target-min-distance*`
------------------------------------
ジャンプ先の間隔をこれより広くする。

※ジャンプ先から数文字分にヒントを表示するのに次のジョウント先までに
十分な隙間が無い場合の処理がテキトーなので、あまり小さな値を設定すると
バグる。

変数 `-jaunte-find-target-function-`
------------------------------------
ジャンプ先を探す関数。
メジャーモードごと等にジョウント先を指定したい場合に使える。

引数なしで繰り返し呼び出されるので、呼び出された時のポイント以後で最初
のジャンプ先とするポイントを integer で返すこと。それ以上ジャンプ先が
無い場合は nil を返すとそこで終了する。


注意点、既知の問題など
======================

バグ報告、質問、要望などは [GitHubIssues] か [@bowbow99] あたりへお願いします。

  [GitHubIssues]: http://github.com/bowbow99/xyzzy.jaunte/issues
  [@bowbow99]: http://twitter.com/bowbow99
