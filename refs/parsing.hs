RawDoc
  (Mapping
    [
      ("key",
        Scalar "value" NoTag Plain Nothing)
      ,
      ("another_key",
        Scalar "Another value goes here." NoTag Plain Nothing)
      ,
      ("a_number_value",
        Scalar "100" NoTag Plain Nothing)
      ,
      ("scientific_notation",
        Scalar "1e+12" NoTag Plain Nothing)
      ,
      ("boolean",
        Scalar "true" NoTag Plain Nothing)
      ,
      ("null_value",
        Scalar "null" NoTag Plain Nothing)
      ,
      ("key with spaces",
        Scalar "value" NoTag Plain Nothing)
      ,
      ("however",
        Scalar "A string, enclosed in quotes." NoTag SingleQuoted Nothing)
      ,
      ("Keys can be quoted too.",
        Scalar "Useful if you want to put a ':' in your key." NoTag DoubleQuoted Nothing)
      ,
      ("single quotes",
        Scalar "have 'one' escape pattern" NoTag SingleQuoted Nothing)
      ,
      ("double quotes",
        Scalar "have many: \", \NUL, \t, \226\152\186, \r\n == \r\n, and more." NoTag DoubleQuoted Nothing)
      ,
      ("Superscript two",
        Scalar "\\u00B2" NoTag Plain Nothing)
      ,
      ("literal_block",
        Scalar "This entire block of text will be the value of the 'literal_block' key,\nwith line breaks being preserved.\n\nThe literal continues until de-dented, and the leading indentation is\nstripped.\n\n    Any lines that are 'more-indented' keep the rest of their indentation -\n    these lines will be indented by 4 spaces.\n" NoTag Literal Nothing)
      ,
      ("folded_style",
        Scalar "This entire block of text will be the value of 'folded_style', but this time, all newlines will be replaced with a single space.\nBlank lines, like above, are converted to a newline character.\n\n    'More-indented' lines keep their newlines, too -\n    this text will appear over two lines.\n" NoTag Folded Nothing)
      ,
      ("a_nested_map",
        Mapping [("key",Scalar "value" NoTag Plain Nothing),("another_key",Scalar "Another Value" NoTag Plain Nothing),("another_nested_map",Mapping [("hello",Scalar "hello" NoTag Plain Nothing)] Nothing)] Nothing)
      ,
      ("0.25",
        Scalar "a float key" NoTag Plain Nothing)
      ,
      ("This is a key\nthat has multiple lines\n",
        Scalar "and this is its value" NoTag Plain Nothing)
      ,
      ("a_sequence",
        Sequence [Scalar "Item 1" NoTag Plain Nothing,Scalar "Item 2" NoTag Plain Nothing,Scalar "0.5" NoTag Plain Nothing,Scalar "Item 4" NoTag Plain Nothing,Mapping [("key",Scalar "value" NoTag Plain Nothing),("another_key",Scalar "another_value" NoTag Plain Nothing)] Nothing,Sequence [Scalar "This is a sequence" NoTag Plain Nothing,Scalar "inside another sequence" NoTag Plain Nothing] Nothing,Sequence [Sequence [Scalar "Nested sequence indicators" NoTag Plain Nothing,Scalar "can be collapsed" NoTag Plain Nothing] Nothing] Nothing] Nothing)
      ,
      ("json_map",
        Mapping [("key",Scalar "value" NoTag DoubleQuoted Nothing)] Nothing)
      ,
      ("json_seq",
        Sequence [Scalar "3" NoTag Plain Nothing,Scalar "2" NoTag Plain Nothing,Scalar "1" NoTag Plain Nothing,Scalar "takeoff" NoTag DoubleQuoted Nothing] Nothing)
      ,
      ("and quotes are optional",
        Mapping [("key",Sequence [Scalar "3" NoTag Plain Nothing,Scalar "2" NoTag Plain Nothing,Scalar "1" NoTag Plain Nothing,Scalar "takeoff" NoTag Plain Nothing] Nothing)] Nothing)
      ,
      ("anchored_content",
        Scalar "This string will appear as the value of two keys." NoTag Plain (Just "anchor_name"))
      ,
      ("other_anchor",
        Alias "anchor_name")
      ,
      ("base",
        Mapping
          [
            ("name",Scalar "Everyone has same name" NoTag Plain Nothing)
            ]
          (Just "base"))
      ,
      ("foo",
        Mapping
          [
            ("<<",Alias "base"),
            ("age",Scalar "10" NoTag Plain Nothing)
            ]
          (Just "foo"))
      ,
      ("bar",
        Mapping
          [
            ("<<",Alias "base"),
            ("age",Scalar "20" NoTag Plain Nothing)
            ]
          (Just "bar"))
      ,
      ("explicit_string",
        Scalar "0.5" StrTag Plain Nothing)
      ,
      ("python_complex_number",
        Scalar "1+2j" (UriTag "tag:yaml.org,2002:python/complex") Plain Nothing)
      ,
      ("datetime",
        Scalar "2001-12-15T02:59:43.1Z" NoTag Plain Nothing)
      ,
      ("datetime_with_spaces",
        Scalar "2001-12-14 21:59:43.10 -5" NoTag Plain Nothing)
      ,
      ("date",
        Scalar "2002-12-14" NoTag Plain Nothing)
      ,
      ("gif_file",
        Scalar "R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5\nOTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+\n+f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC\nAgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=\n" (UriTag "tag:yaml.org,2002:binary") Literal Nothing)
      ,
      ("set",
        Mapping
          [
            ("item1",Scalar "" NoTag Plain Nothing),
            ("item2",Scalar "" NoTag Plain Nothing),
            ("item3",Scalar "" NoTag Plain Nothing)
            ]
          Nothing)
      ,
      ("or",
        Mapping
          [
            ("item1",Scalar "" NoTag Plain Nothing),
            ("item2",Scalar "" NoTag Plain Nothing),
            ("item3",Scalar "" NoTag Plain Nothing)
            ]
          Nothing)
      ,
      ("set2",
        Mapping
          [
            ("item1",Scalar "null" NoTag Plain Nothing),
            ("item2",Scalar "null" NoTag Plain Nothing),
            ("item3",Scalar "null" NoTag Plain Nothing)
            ]
          Nothing)
      ]
    Nothing)
    (fromList [
      ("anchor_name",Scalar "This string will appear as the value of two keys." NoTag Plain (Just "anchor_name")),
      ("bar",Mapping [("<<",Alias "base"),
      ("age",Scalar "20" NoTag Plain Nothing)] (Just "bar")),
      ("base",Mapping [("name",Scalar "Everyone has same name" NoTag Plain Nothing)] (Just "base")),
      ("foo",Mapping [("<<",Alias "base"),
      ("age",Scalar "10" NoTag Plain Nothing)] (Just "foo"))
      ])