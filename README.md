Rokka
=====

	

    Rokka System Specification Ver. 2013/09/25 12JST　Copyright (c) N.T. Technology, Inc.
     
    Rokka System is the new method to get 2ch/bbspink archived dats.
     
    Rokka retrieves dat files:
    　archived 2ch dat files
    　archived bbspink dat files
    　pooled 2ch dat files
    　pooled bbspink dat files
    　live 2ch dat files
    　live bbspink dat files
     
    Remark:
    　live dat - a thread where you can post and listed on subject.txt
    　pooled dat - a thread where you can not post and not listed on subject.txt, it will be archived soon
    　archived dat - a thread archived
     
     
    KAGI
    To get a dat from Rokka, you need to get KAGI from authentication server.
     
    　https://2chv.tora3.net/futen.cgi?ID=<User ID>&PW=<Password>
     
    When authentication is succeeded , KAGI is returned like;
    Monazilla/2.00:4373298c8948y4671g4635r53615D4699f4014I3455C9148A6600f2811s45242k42852u6725y95346g6820L6383H0297o62124l2450n64672G6826N2472L7957N2508x9686O8904U4108793x6855v1216b1499s6811a2729r
     
     
    Getting a dat
    You can get a dat from Rokka using KAGI.
     
    　http://rokka.<DOMAIN>/<SERVER>/<BOARD>/<THREAD>/[<OPTIONS>]?sid=<KAGI>
     
    　DOMAIN : 2ch.net or bbspink.com
    　SERVER : name of the server, pele,kilauea,...(bbspink)　anago,awabi,....(2ch)
    　BOARD : name of the board, news,entrance,.....
    　THREAD : thread key(=thread number)
    　OPTIONS : 2ch standard url options, l50 , 25-35 , -45 , 13- , etc...
    　　　　　　if the range is out of the thread, whole posts will be sent
    　<KAGI> : authenticated KAGI, UrlEncoding is recommended
     
    　Response : 1st line indicates processed status of the server.
    　　"Success XXX"　- The process has successfuly done. XXX shows where the dat is retrieved from
    　　　　　　　　　　　Live　　　　
    　　　　　　　　　　　Pool　　　　
    　　　　　　　　　　　Archive 　　
    　　　　　　　　　　 Following lines are posts formatted with 2ch dat format(name<>email<>datetime<>body<>[title]).
    　　"Error XXX"　　- The process has not succeeded. XXX is error code.
    　　　　　　　　　　 Error codes:  
    　　　　　　　　　　　13 　　　not found　　　　　　the requested dat not found
    　　　　　　　　　　　8008135　inputError 　　　　　invalid SERVER or BOARD or THREAD
    　　　　　　　　　　　666　　　urlError 　　　　　　invalid OPTIONS
    　　　　　　　　　　　69 　　　authenticationError　invalid KAGI
    　　　　　　　　　　　420　　　timeLimitError 　　　access too fast, interval between requests required
     
    Rokka also returns standard HTTP status codes in the HTTP response header: 404, 200, 401, 403 etc.
     
    You can get gzipped content specifying Accept-Encoding:gzip in the HTTP request header.
     
    Note:
    　Error 420
    　When you make too many requests per second, per minute, or per hour, then rokka will stop you until you slow down.
    　This is to prevent automated spidering of the dat files.
    　The current time limits are;
    　　10 dats within 1 second
    　　60 dats within 1 minute
    　　600 dats within 1 hour
    　When you try to get more dats over the limitation, you will get Error 420.
     
     
    Source Code:
    https://github.com/Cipherwraith/Rokka
     
    Discussion BBS:
    Rokka System
    http://pele.bbspink.com/test/read.cgi/erobbs/1379086553/
     
     
    2013/09/25 Code Monkey @ N.T. Technology, Inc.
     
     
     
    Rokkaシステム仕様書 Ver. 2013/09/25 12JST　Copyright (c) N.T. Technology, Inc.
     
    Rokkaシステムは２ちゃん/BBSPINKのDAT落ちログを取得する新しい方法です。
     
    Rokkaからは以下のdatログを取得する事ができます：
    　２ちゃん過去ログ
    　bbspink過去ログ
    　２ちゃんdat落ちログ
    　bbspink dat落ちログ
    　２ちゃんライブスレッド
    　bbspinkライブスレッド
     
    注:
    　ライブスレッド - subject.txtに載っていて書き込み可能なスレッド
    　dat落ちスレッド - subject.txtに載っていなく書き込み不可能なスレッド、じきに過去ログ化されます
    　過去ログ - 板移転前からのログを蓄積したものです　一部失われているものもあります
     
     
    KAGI
    Rokkaからdatを取得するには「KAGI」を認証サーバーより取得する必要があります。
     
    　https://2chv.tora3.net/futen.cgi?ID=<User ID>&PW=<Password>
     
    認証に成功すると以下のようなKAGIが取得できます。
    Monazilla/2.00:4373298c8948y4671g4635r53615D4699f4014I3455C9148A6600f2811s45242k42852u6725y95346g6820L6383H0297o62124l2450n64672G6826N2472L7957N2508x9686O8904U4108793x6855v1216b1499s6811a2729r
     
     
    datの取得
    取得したKAGIを使ってRokkaからdatを取得します。
     
    　http://rokka.<DOMAIN>/<SERVER>/<BOARD>/<THREAD>/[<OPTIONS>]?sid=<KAGI>
     
    　DOMAIN : 2ch.net または bbspink.com
    　SERVER : サーバー名, pele,kilauea,...(bbspink)　anago,awabi,....(2ch)
    　BOARD : スレッド名, news,entrance,.....
    　THREAD : スレッドキー
    　OPTIONS : ２ちゃんの標準URLオプションです, l50 , 25-35 , -45 , 13- , etc...
    　　　　　　範囲外の指定だった場合スレッド全体が返ります
    　<KAGI> : 取得したKAGIです　URLエンコード推奨
     
    　レスポンス : 1行目にrokkaの処理結果が記述されます
    　　"Success XXX"　- 成功　XXXにdatの状態（取得元）が記述されます
    　　　　　　　　　　　Live　　　　ライブスレッド
    　　　　　　　　　　　Pool　　　　dat落ちスレッド
    　　　　　　　　　　　Archive 　　過去ログ
    　　　　　　　　　　 以降の行にDAT形式(name<>email<>datetime<>body<>[title])でログが記述されています
    　　"Error XXX"　　- 何らかのエラーです　XXX がエラーコードです。
    　　　　　　　　　　　13 　　　not found　　　　　　要求されたdatが見つかりませんでした
    　　　　　　　　　　　8008135　inputError 　　　　　リクエストURLのSERVERかBOARDかTHREADが正しくないです
    　　　　　　　　　　　666　　　urlError 　　　　　　OPTIONSが正しくないです
    　　　　　　　　　　　69 　　　authenticationError　KAGIが不正（有効期限切れその他）
    　　　　　　　　　　　420　　　timeLimitError 　　　アクセス間隔が短すぎます
     
    HTTPステータスコード（404,200など）もHTTPレスポンスヘッダに同時に返します。
     
    HTTPリクエストヘッダーにAccept-Encoding:gzipを含めるとレスポンスをGZipで返します。
     
    注:
    　エラーError 420について
    　クローラー対策のため1秒あたり、1分あたり、1時間あたりの合計取得数には制限があり、これを過ぎるとこのエラーが返ります。
    　制限値は以下の通りです。
    　　1秒間に10スレまで
    　　1分間に60スレまで
    　　1時間に600スレまで
    　この制限値以上スレを取得しようとするとError 420が返ってきます。
     
     
    ソースコード:
    https://github.com/Cipherwraith/Rokka
     
    議論スレ:
    Rokka System
    http://pele.bbspink.com/test/read.cgi/erobbs/1379086553/
     
    2013/09/25 Code Monkey @ N.T. Technology, Inc.
    　　　　　　翻訳　水玉(Mizutama) ◆qHK1vdR8FRIm


