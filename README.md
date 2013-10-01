Rokka
=====


	Rokkaシステム仕様書 Ver. 2013/09/27 02JST　Copyright (c) N.T. Technology, Inc.

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

	認証に成功すると次のようなレスポンスが返ります。
	SESSION-ID=Monazilla/2.00:4373298c8948y4671g4635r53615D4699f4014I3455C9148A6600f2811s45242k42852u6725y95346g6820L6383H0297o62124l2450n64672G6826N2472L7957N2508x9686O8904U4108793x6855v1216b1499s6811a2729r
	認証に失敗すると次のようなレスポンスが返ります。
	SESSION-ID=ERROR:pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp

	認証に成功した場合、'SESSION-ID='直後の192バイトが KAGI となります。
	Monazilla/2.00:4373298c8948y4671g4635r53615D4699f4014I3455C9148A6600f2811s45242k42852u6725y95346g6820L6383H0297o62124l2450n64672G6826N2472L7957N2508x9686O8904U4108793x6855v1216b1499s6811a2729r

	00000000  53 45 53 53 49 4F 4E 2D 49 44 3D 4D 6F 6E 61 7A   SESSION-ID=Monaz
	00000010  69 6C 6C 61 2F 32 2E 30 30 3A 34 33 37 33 32 39   illa/2.00:437329
	00000020  38 63 38 39 34 38 79 34 36 37 31 67 34 36 33 35   8c8948y4671g4635
	00000030  72 35 33 36 31 35 44 34 36 39 39 66 34 30 31 34   r53615D4699f4014
	00000040  49 33 34 35 35 43 39 31 34 38 41 36 36 30 30 66   I3455C9148A6600f
	00000050  32 38 31 31 73 34 35 32 34 32 6B 34 32 38 35 32   2811s45242k42852
	00000060  75 36 37 32 35 79 39 35 33 34 36 67 36 38 32 30   u6725y95346g6820
	00000070  4C 36 33 38 33 48 30 32 39 37 6F 36 32 31 32 34   L6383H0297o62124
	00000080  6C 32 34 35 30 6E 36 34 36 37 32 47 36 38 32 36   l2450n64672G6826
	00000090  4E 32 34 37 32 4C 37 39 35 37 4E 32 35 30 38 78   N2472L7957N2508x
	000000A0  39 36 38 36 4F 38 39 30 34 55 34 31 30 38 37 39   9686O8904U410879
	000000B0  33 78 36 38 35 35 76 31 32 31 36 62 31 34 39 39   3x6855v1216b1499
	000000C0  73 36 38 31 31 61 32 37 32 39 72 0A 0A            s6811a2729r..

	HEAD/GET/POSTの各HTTPメソッドが使用できます。


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
	　　　　　　　　　　　8008135　inputError 　　　　　リクエストURLのSERVERかBOARDが正しくないです
	　　　　　　　　　　　666　　　urlError 　　　　　　OPTIONSまたはQueryStringが正しくないです
	　　　　　　　　　　　69 　　　authenticationError　KAGIが不正（有効期限切れその他）
	　　　　　　　　　　　420　　　timeLimitError 　　　アクセス間隔が短すぎます
	　　　　　　　　　　　42 　　　methodError　　　　　そのHTTPメソッドは許可されていません

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

	注:
	　HTTP GETとHEADのメソッドのみ有効です。

	注:
	HTTP status code        response   Value      Description
	                        1st line
	200 Success             Success    Live       ライブスレッド
	                                   Pool       dat落ちスレッド
	                                   Archived   過去ログ
	400 Bad Request         Error      
	401 Unauthorized                   69         KAGIが不正（有効期限切れその他）
	                                   420        アクセス間隔が短すぎます
	                                   666        OPTIONSまたはQueryStringが正しくないです
	403 Forbidden                      
	404 Not Found                      13         要求されたdatが見つかりませんでした
	                                   8008135    リクエストURLのSERVERかBOARDが正しくないです
	405 Method Not Allowed             42         そのHTTPメソッドは許可されていません


	ソースコード:
	https://github.com/Cipherwraith/Rokka

	議論スレ:
	Rokka System
	http://pele.bbspink.com/test/read.cgi/erobbs/1379086553/

	2013/09/25 Code Monkey @ N.T. Technology, Inc.
	　　　　　　翻訳　水玉(Mizutama) ◆qHK1vdR8FRIm








	Rokka System Specification Ver. 2013/09/27 02JST  Copyright (c) N.T. Technology, Inc.

	Rokka System is the new method to get 2ch/bbspink archived dats. If the English and Japanese version of this document are different, then the English version is the correct version.

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

	When authentication is succeeded ,
	SESSION-ID=Monazilla/2.00:4373298c8948y4671g4635r53615D4699f4014I3455C9148A6600f2811s45242k42852u6725y95346g6820L6383H0297o62124l2450n64672G6826N2472L7957N2508x9686O8904U4108793x6855v1216b1499s6811a2729r
	When failed ,
	SESSION-ID=ERROR:pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp

	KAGI is 192 bytes after 'SESSION-ID=' when authentication is succeeded.
	Monazilla/2.00:4373298c8948y4671g4635r53615D4699f4014I3455C9148A6600f2811s45242k42852u6725y95346g6820L6383H0297o62124l2450n64672G6826N2472L7957N2508x9686O8904U4108793x6855v1216b1499s6811a2729r

	00000000  53 45 53 53 49 4F 4E 2D 49 44 3D 4D 6F 6E 61 7A   SESSION-ID=Monaz
	00000010  69 6C 6C 61 2F 32 2E 30 30 3A 34 33 37 33 32 39   illa/2.00:437329
	00000020  38 63 38 39 34 38 79 34 36 37 31 67 34 36 33 35   8c8948y4671g4635
	00000030  72 35 33 36 31 35 44 34 36 39 39 66 34 30 31 34   r53615D4699f4014
	00000040  49 33 34 35 35 43 39 31 34 38 41 36 36 30 30 66   I3455C9148A6600f
	00000050  32 38 31 31 73 34 35 32 34 32 6B 34 32 38 35 32   2811s45242k42852
	00000060  75 36 37 32 35 79 39 35 33 34 36 67 36 38 32 30   u6725y95346g6820
	00000070  4C 36 33 38 33 48 30 32 39 37 6F 36 32 31 32 34   L6383H0297o62124
	00000080  6C 32 34 35 30 6E 36 34 36 37 32 47 36 38 32 36   l2450n64672G6826
	00000090  4E 32 34 37 32 4C 37 39 35 37 4E 32 35 30 38 78   N2472L7957N2508x
	000000A0  39 36 38 36 4F 38 39 30 34 55 34 31 30 38 37 39   9686O8904U410879
	000000B0  33 78 36 38 35 35 76 31 32 31 36 62 31 34 39 39   3x6855v1216b1499
	000000C0  73 36 38 31 31 61 32 37 32 39 72 0A 0A            s6811a2729r..

	HTTP HEAD/GET/POST method can be used.



	Getting a dat
	You can get a dat from Rokka using KAGI.

	  http://rokka.<DOMAIN>/<SERVER>/<BOARD>/<THREAD>/[<OPTIONS>]?sid=<KAGI>

	  DOMAIN : 2ch.net or bbspink.com 
	  SERVER : name of the server, pele,kilauea,...(bbspink)  anago,awabi,....(2ch) 
	  BOARD : name of the board, news,entrance,..... 
	  THREAD : thread key(=thread number) 
	  OPTIONS : 2ch standard url options, l50 , 25-35 , -45 , 13- , etc... 
	            if the range is out of the thread, whole posts will be sent
	  <KAGI> : authenticated KAGI, UrlEncoding is recommended 
	 
	  Response : 1st line indicates processed status of the server. 
	    "Success XXX"  - The process has successfuly done. XXX shows where the dat is retrieved from
	                      Live        
	                      Pool        
	                      Archive     
	                     Following lines are posts formatted with 2ch dat format(name<>email<>datetime<>body<>[title]).
	    "Error XXX"    - The process has not succeeded. XXX is error code. 
	                     Error codes:  
	                      13       not found            the requested dat not found
	                      8008135  inputError           invalid SERVER or BOARD
	                      666      urlError             invalid OPTIONS or QueryString
	                      69       authenticationError  invalid KAGI
	                      420      timeLimitError       access too fast, interval between requests required 
	                      42       methodError          method not allowed

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

	Note:
	  HTTP GET and HEAD can be used.

	Note:
	HTTP status code        response   Value      Description
	                        1st line
	200 Success             Success    Live       the dat is live thread
	                                   Pool       the dat in not live and not archived
	                                   Archived   the dat is archived
	400 Bad Request         Error      
	401 Unauthorized                   69         invalid KAGI, expired or illegal
	                                   420        access too fast
	                                   666        invalid OPTIONS or QueryString
	403 Forbidden                      
	404 Not Found                      13         invalid THREAD
	                                   8008135    invalid SERVER/BOARD
	405 Method Not Allowed             42         invalid HTTP method


	Source Code:
	https://github.com/Cipherwraith/Rokka

	Discussion BBS:
	Rokka System
	http://pele.bbspink.com/test/read.cgi/erobbs/1379086553/


	2013/09/25 Code Monkey @ N.T. Technology, Inc.
