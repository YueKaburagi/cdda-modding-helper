
# cdda-modding-helper
これは CacaclysmDDA の mod をつくってるときに出た   
- 何度も同じの書きたくないなー   
- 仕様も頻繁に変化するからガチガチに固めてもtoolもmodもメンテ面倒だなー   
- (普段日本語設定でやってるから)いざmod作ろうとしたときにアイテム名とか探すのめんどいなー   

といった発想から作成されています    

自分用だったのでいろいろ雑です。領域外チェックとか疎かです     
[cdda-anything-i-needs](https://github.com/YueKaburagi/cdda-anything-i-needs) で実際に使ってます


## __config.json
幾つかの機能を利用するには config設定が必要になります   
__config.json はカレントディレクトリに置いてください   
```json
{
    "default_version": "#6024",
    "#6024": {
        "destination": "/path/to/cdda/data/mods",
	"cdda_json_root": "/path/to/cdda/data/json",
	"po_path": "/path/to/po_file.po",
    },
    "#5934": {
      ...
}
```
**"cdda_json_root"**は  
- json変換の 読み込み/import機能  
- 簡易ブラウザ機能の `find`  

を  

**"po_path"**は  
- 簡易ブラウザ機能の `lookup`  

を  
それぞれ利用するために必要です  

"po_path"で指定するpoファイルは `msgunfmt` などを使って予めmoファイルから変換しておいてください   
moファイルは cdda/lang/ 以下にあると思います   

## json変換 v1.0.0
実行時引数で指定されたディレクトリ以下にあるjsonファイルを変換します   
`cdda-modding-helper <targetDir> [destDir]`    
`destDir`が省略された場合は、\_\_config.json の`destination`を参照します     
`destination` も存在しなければ `targetDir/transformed/` に出力します

処理順は   
template → import → replace    
です   

### テンプレート/template v1.0.0
*\_\_template.json で指定されたテンプレートを繋ぎ合わせて出力します*   

```json
{
    "recipe": {
        "type": "recipe",
        "autolearn": true,
        "reversible": false
    }
}
```
__template.json にこのようにある場合、以下のJSONオブジェクトは   
```json
{
    "__templates": ["recipe"],
    "result": "leather"
}
```
  ↓
```json
{
    "type": "recipe",
    "autolearn": true,
    "reversible": false,
    "result": "leather"
}
```
このようにマージされます   

テンプレート指定は階層化や複数継承が可能です   
より若く指定されたテンプレートが優先されてマージされます   

### 読み込み/import v1.4.0
*cdda本体に同梱されている様なjsonオブジェクトを流用します*       
__config.json に `"json_root_dir"` の指定が必要です   
以下のようにある場合、ragのレシピをjson_root_dir以下から探して、そのオブジェクト定義で参照できるようにします   
```json
{
    "__import": [{
        "ref": {
            "type": "recipe",
            "result": "rag",
            "id_suffix": "__undefined",
        },
        "bind": "obj",
        "import": "as template",
        "ignore": ["batch_time_factors"]
    }],
```
- **"ref"** では参照するオブジェクトを一意に探し出せる、キーと値の1つ以上の組を与えます   
 この際値として `"__undefined"` を指定すると、そのキーを持たないオブジェクトにマッチします   
 (↑の例では type=recipe result=rag だけでは1つに絞り込めないので、id_suffix=__undefined を与えています)   
- **"bind"** ではそのオブジェクトを参照するための識別子を定義します   
 `"this"` は特殊な用途に使えるよう予約済みとなっています   
- **"import"** では参照したオブジェクトをテンプレートの様にマージするかどうかを指定します   
 `"as template"` の場合マージし、`"bind only"` の場合はマージしません   
- **"ignore"** では参照したオブジェクトの特定のフィールドを読み込まないようにします
  
__import で読み込んだオブジェクトを、その定義中に利用することができます   
値として `"${...}"` が与えられた場合、その内部を式として評価します   
```json
    "difficulty": "${obj.difficulty + 2}",
    "tools": "${obj.tools ++ this.__tools}",
    "__tools": [ [[ "lighter", 1 ]] ]
}
```
obj.key とある場合は、obj に対応するオブジェクトの key の値を指します    
obj の代わりに this を使用した場合は、それはちょうど定義中の自身を指します   

\+ や ++ など、幾つかの二項演算子を提供しています   
現在あるのは    
- 算術計算  + - * 
- 配列結合  ++     

のみです   


### 置換/replace v1.1.0
*\_\_replace.json で指定されたルールに従って、jsonファイルを書き換えます*   

__replace.json  
```json
{
    "__name": ["__name_ja","__name_en"],
    "__description": "__description_ja",
}
```

incomplete.json   
```json
{
    "name": "__name",
    "description": "__description",
    "__name_en": "test item",
    "__description_ja": "ツールのテストです"
}
```
__replace定義と、オブジェクト定義が↑のようにある場合   
これを以下のように変換します   
```json
{
    "name": "test item",
    "description": "ツールのテストです"
}
```

__replace定義でキーに対応する値を配列で与えたときは、若い方から検索して最初に見つかった置換先を利用します   
キーと値が1:1の場合は単一の候補だけを対象とします  
どちらの場合でも置換先が未定義の場合 warning を表示してそのままにします   

### おまけ
接頭辞/prefix "\_\_" が付けられているキーは最後に全部消されるので、   
replace や import での一時利用フィールドとして "\_\_" からはじまる文字列をキーに指定するのがオススメです   

## 簡易ブラウザ v1.2.0
find 機能を使う場合は __config.json に "json_root_dir" の指定が   
lookup 機能を使う場合は それに加えて "po_path" の指定が     
それぞれ必要です   
これらは実行時引数として渡すことも可能です   
`cdda-modding-helper -b [<cdda/data/json/> [<***.po>]]`   

### command / コマンド

- `find [option|quantifier...]`   
 json_root_dir 以下の全部のjsonファイルにあるオブジェクトから検索します   
- `lookup [option|quantifier...] <string> [option|quantifier...]`   
 find の機能に加えて、poファイルを見て逆翻訳した値での検索を行えます   
 正確には「入力文字列を含む訳語に対応した英語を探し、その英語を値に持つオブジェクトを検索する」です   
- `exit`   
 終了する   

### quantifier / 限定子
- `key=value`    
 指定したキーと値の組み合せを持つオブジェクトを対象にする   
- `key=`   
 指定したキーを持つオブジェクトを対象にする    
- `=value`    
 指定した値を持つオブジェクトを対象にする   
- `name`   
 [lookupのみ] poファイルの中でも名前っぽいものだけを対象にする   
- `item`   
 アイテムのみを対象にする `volume=`と等価   
- `no <quantifier>`   
 直後の限定子を否定する   

### option / オプション   
- `up to <number>`   
 検索結果の最大表示数を変更する デフォルトでは25   
- `id`   
 見つけたオブジェクトのidの値のみを表示する   


## 外部mod翻訳補助
*対象のMod内にある `name` や `description` などをリストアップし、poファイル をつくります*       
`cdda-modding-helper -p <targetModPath> [outFile]`       
出力先の指定がない場合はカレントディレクトリに出力されます        


この poファイル は完全でないので、msgcat で以下のように指定して繋ぎ合わせる必要があります       
`msgcat --use-first -o <outFile.po> <master.po> <partial.po>`      

大まかな流れとしては     
1. cdda同梱の moファイル を `msgunfmt` などで poファイル に変換する       
**`msgunfmt cataclysm-dda.mo -o master.po`**            
2. `cdda-modding-helper` を `-p` モードで実行する        
**`java -jar cdda-modding-helper.jar -p ModWrittenInEnglish partial.po`**          
3. 出力された poファイル を翻訳する       
4. cddaの poファイル に、modの poファイル を `msgcat` する         
**`msgcat --use-first master.po partial.po -o new.po`**       
5. `msgfmt` で繋げた poファイル を moファイル に変換する      
**`msgfmt new.po -o cataclysm-dda.mo`**    
6. cddaの所定の位置に moファイル を上書きする     

といった感じになります      
