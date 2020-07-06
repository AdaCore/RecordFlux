"""
RecordFlux support for GNAT Studio
"""

import os.path
import re

import GPS
import highlighter.common as hl
from gs_utils import hook

XML = r"""<?xml version="1.0"?>
<GNAT_Studio>
   <Language>
      <Name>RecordFlux</Name>
      <Body_Suffix>.rflx</Body_Suffix>

      <Categories>
         <Category>
            <Name>package</Name>
            <Pattern>^[ \\t]*package[ \\t]+((\\w|\\.)+)</Pattern>
            <Index>1</Index>
         </Category>
         <Category>
            <Name>type</Name>
               <Pattern>^[ \\t]*type[ \\t]+(\\w+)</Pattern>
               <Index>1</Index>
         </Category>
      </Categories>

      <Context>
         <New_Line_Comment_Start>--</New_Line_Comment_Start>
         <String_Delimiter>&quot;</String_Delimiter>
         <Can_Indent>True</Can_Indent>
         <Syntax_Highlighting>True</Syntax_Highlighting>
         <Case_Sensitive>False</Case_Sensitive>
      </Context>
   </Language>

   <!-- Filter -->
   <filter name="RecordFlux" language="RecordFlux"/>

   <!-- Actions -->
   <action name="check">
      <filter_and>
         <filter id="RecordFlux"/>
         <filter id="Source editor"/>
      </filter_and>
      <external>rflx check %F</external>
      <on-failure>
         <shell lang="python" show-command="false">recordflux.parse_output(&quot;&quot;&quot;%1&quot;&quot;&quot;)</shell>
      </on-failure>
   </action>

   <action name="check_all">
      <filter id="File"/>
      <shell lang="python" show-command="false">recordflux.get_source_files()</shell>
      <external>rflx check %1"</external>
      <on-failure>
         <shell lang="python" show-command="false">recordflux.parse_output(&quot;&quot;&quot;%1&quot;&quot;&quot;)</shell>
      </on-failure>
   </action>

   <action name="generate">
      <filter_and>
         <filter id="RecordFlux"/>
         <filter id="Source editor"/>
      </filter_and>
      <external>rflx generate -d generated %F</external>
      <on-failure>
         <shell lang="python" show-command="false">recordflux.parse_output(&quot;&quot;&quot;%1&quot;&quot;&quot;)</shell>
      </on-failure>
   </action>

   <action name="generate_all">
      <filter_and>
         <filter id="File"/>
      </filter_and>
      <shell lang="python" show-command="false">recordflux.get_source_files()</shell>
      <external>rflx generate -d generated %1"</external>
      <on-failure>
         <shell lang="python" show-command="false">recordflux.parse_output(&quot;&quot;&quot;%1&quot;&quot;&quot;)</shell>
      </on-failure>
   </action>

   <!-- Aliases -->
   <alias name="rflx_package">
      <param name="name" description="The name of the RecordFlux package"/>
      <text>package %(name) is
begin
   %_
end %(name);</text>
   </alias>

   <!-- Submenu -->
   <submenu before="Window">
      <title>RecordFlux</title>

      <menu action="check">
         <title>Check</title>
      </menu>

      <menu action="check_all">
         <title>Check All</title>
      </menu>

      <menu action="generate">
         <title>Generate</title>
      </menu>

      <menu action="generate_all">
         <title>Generate All</title>
      </menu>
   </submenu>

</GNAT_Studio>
"""

GPS.parse_xml(XML)

# Highlighting
recordflux_keywords = [
    "and",
    "array of",
    "end",
    "for",
    "if",
    "is",
    "message",
    "mod",
    "new",
    "null",
    "or",
    "package",
    "range",
    "then",
    "type",
    "use",
    "with",
]

recordflux_literals = [
    "False",
    "True",
]

tag_aspect = hl.existing_style("Src-Editor-Aspects-Variant", "aspects")

hl_type = hl.simple(r"\b[^;\s]+", tag=hl.tag_type)
type_region = hl.region(r":", r"\b", highlighter=(hl_type,), tag=hl.tag_default)
string_literal = hl.region(r'"', r'"', matchall=False, tag=hl.tag_string)

hl.register_highlighter(
    language="recordflux",
    spec=(
        hl.simple(r"--[^\n]*", tag=hl.tag_comment),
        type_region,
        hl.simple(r"\b'First", tag=tag_aspect),
        hl.simple(r"\b'Last", tag=tag_aspect),
        hl.simple(r"\b'Length", tag=tag_aspect),
        hl.simple(r"\b'Size", tag=tag_aspect),
        hl.simple(r"\bFirst\s+=>", tag=tag_aspect),
        hl.simple(r"\bLast\s+=>", tag=tag_aspect),
        hl.simple(r"\bLength\s+=>", tag=tag_aspect),
        hl.simple(r"\bSize\s+=>", tag=tag_aspect),
        hl.words(recordflux_keywords, tag=hl.tag_keyword),
        hl.words(recordflux_literals, tag=hl.tag_keyword),
        hl.simple(r"16#[_A-Fa-f0-9]+#", tag=hl.tag_number),
        hl.simple(r"\b[_0-9]+\b", tag=hl.tag_number),
        hl.words(("Always_Valid"), tag=tag_aspect),
        string_literal,
    ),
)


@hook("gps_started")
def __on_gps_started():
    GPS.FileTemplate.register(
        alias_name="rflx_package",
        label="RecordFlux specification",
        unit_param="name",
        language="RecordFlux",
        is_impl=False,
    )


message_re = re.compile(
    r"^"
    r"(?P<filename>[^:]+):"
    r"(?P<line>\d+):"
    r"(?P<column>\d+): "
    r"(?P<subsystem>core|parser|model|cli|internal|graph): "
    r"(?P<severity>info|warning|error): "
    r"(?P<message>.*)"
    r"$"
)

generic_message_re = re.compile(
    r"^"
    r"(?P<subsystem>core|parser|model|cli|internal|graph): "
    r"(?P<severity>info|warning|error): "
    r"(?P<message>.*)"
    r"$"
)


def to_importance(severity):
    if severity == "error":
        return GPS.Message.Importance.HIGH
    if severity == "warning":
        return GPS.Message.Importance.MEDIUM
    if severity == "info":
        return GPS.Message.Importance.INFORMATIONAL


def parse_output(output):
    for m in GPS.Message.list():
        m.remove()
    message = None
    for l in output.splitlines():
        result = message_re.match(l)
        if result:
            data = result.groupdict()
            if data["severity"] != "info":
                message = GPS.Message(
                    category="RecordFlux",
                    file=GPS.File(data["filename"]),
                    line=int(data["line"]),
                    column=int(data["column"]),
                    text="{subsystem}: {severity}: {message}".format(**data),
                    importance=to_importance(data["severity"]),
                )
            elif message:
                data["relative"] = os.path.basename(data["filename"])
                message.create_nested_message(
                    file=GPS.File(data["filename"]),
                    line=int(data["line"]),
                    column=int(data["column"]),
                    text="{message} ({relative}:{line}:{column})".format(**data),
                )
            continue

        result = generic_message_re.match(l)
        if result:
            data = result.groupdict()
            message = GPS.Message(
                category="RecordFlux",
                file=GPS.File(data["subsystem"]),
                line=1,
                column=1,
                text="{subsystem}: {severity}: {message}".format(**data),
                importance=to_importance(data["severity"]),
            )


def get_source_files():
    files = (
        GPS.current_context().project().sources()
        if GPS.current_context().project()
        else GPS.current_context().files()
    )

    if files:
        return " ".join([s.name() for s in files if s.language() == "recordflux"])

    raise GPS.Exception("No files found")
