{ outputs = { ... }:
    with builtins;
    { __functor = _: { lib }:
        let l = lib; in
        rec
        { string =
            { splitAt = pos: str:
                let chars = l.stringToCharacters str; in
                { before = l.concatStrings (l.take pos chars);
                  after = l.concatStrings (l.drop pos chars);
                };

              uncons = str:
                if str == "" then
                  maybe.nothing
                else
                  let chars = l.stringToCharacters str; in
                  maybe.just
                    { head = head chars;
                      tail = l.concatStrings (tail chars);
                    };
            };

          maybe =
            { just = a: (j: _: j a);
              nothing = (_: n: n);
            };

          either =
            rec
            { left = a: (l: _: l a);
              right = a: (_: r: r a);

              map = f: value:
                value
                  left
                  (a: right (f a));

              join = value:
                value
                  left
                  (value':
                     value'
                       left
                       right
                  );

              bind = value: f: join (map f value);
              getLeft = default: value: value l.id (_: default);
              getRight = default: value: value (_: default) l.id;
            };

          parser =
            { map = f: p:
                s:
                  either.map
                    ({ result, suffix }: { result = f result; inherit suffix; })
                    (p s);

              bind = p: f:
                s:
                  either.bind
                    (p s)
                    ({ result, suffix }:
                       f result suffix
                    );

              pure = a:
                s: either.right { result = a; suffix = s; };

              empty = parser.fail "No alternative";

              alt = p1: p2:
                s:
                  (p1 s)
                    ({ error, pos }:
                       if s.position == pos
                       then p2 s
                       else either.left { inherit error pos; }
                    )
                    either.right;

              oneOf = l.foldr parser.alt parser.empty;

              run = p: substring:
                either.map (a: a.result) (p { position = 0; inherit substring; });

              anyChar = { substring, position }:
                (string.uncons substring)
                  ({ head, tail }:
                     either.right
                       { result = head;
                         suffix = { substring = tail; position = position + 1; };
                       }
                  )
                  (either.left { pos = position; error = "Unexpected EOF"; });

              try = p:
                s:
                  (p s)
                    ({ error, ... }: either.left { pos = s.position; inherit error; })
                    either.right;

              fail = error:
                { position, ... }: either.left { pos = position; inherit error; };

              satisfy = pred:
                parser.try
                  (parser.bind parser.anyChar
                     (c:
                        if pred c then parser.pure c
                        else parser.fail "Character '${c}' did not satisfy predicate"
                     )
                  );

              string = str:
                { substring, position }:
                  let
                    length = stringLength str;
                    inherit (string.splitAt length substring) before after;
                  in
                  if before == str then
                    either.right
                      { result = str;
                        suffix = { substring = after; position = position + length; };
                      }
                  else
                    either.left { pos = position; error = "Expected '${str}'."; };

              assertConsume = p:
                s:
                  (p s)
                    either.left
                    (result:
                       if s.position < result.suffix.position
                       then either.right result
                       else either.left { pos = s.position; error = "Consumed no input."; }
                    );

              many = p:
                parser.alt
                  (parser.bind (parser.assertConsume p)
                     (a:
                        parser.bind
                          (parser.many p)
                          (b: parser.pure ([ a ] ++ b))
                     )
                  )
                  (parser.pure []);

              surround = str: wrapper:
                parser.bind
                  (parser.string str)
                  (_: parser.bind
                        (parser.many
                           (parser.try
                              (parser.alt
                                 (parser.bind
                                    (parser.string str)
                                    (_: parser.fail "'${str}' not allowed")
                                 )
                                 (parser.anyChar)
                              )
                           )
                        )
                        (innerChars:
                           parser.map
                             (l.const (wrapper (l.concatStrings innerChars)))
                             (parser.string str)
                        )
                  );
            };
        };
    };
}
