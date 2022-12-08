DEFINITE_MESSAGE_WITH_BUILTIN_TYPE_SPEC = """\
       package Test is

          type Length is range 0 .. 2 ** 7 - 1 with Size => 7;

          type Message is
             message
                Flag : Boolean;
                Length : Length
                   if Length > 0;
                Data : Opaque
                   with Size => Length * 8;
             end message;

       end Test;
        """

PARAMETERIZED_MESSAGE_SPEC = """\
        package Test is

           type Length is range 1 .. 2 ** 14 - 1 with Size => 16;

           type Message (Length : Length; Extended : Boolean) is
              message
                 Data : Opaque
                    with Size => Length * 8
                    then Extension
                        if Extended = True
                    then null
                        if Extended = False;
                 Extension : Opaque
                    with Size => Length * 8;
              end message;

        end Test;
        """

DEFINITE_PARAMETERIZED_MESSAGE_SPEC = """\
        package Test is

           type Length is range 1 .. 2 ** 14 - 1 with Size => 16;

           type Message (Length : Length) is
              message
                 Data : Opaque
                    with Size => Length * 8;
              end message;

        end Test;
        """
