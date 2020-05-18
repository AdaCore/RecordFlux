"""RecordFlux support for GNAT Studio
"""


import GPS
import gs_utils.gnat_rules
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
   <filter name="is_recordflux" language="RecordFlux"/>

   <!-- Actions -->
   <action name="check">
      <filter id="is_recordflux"/>
      <external>rflx check %F</external>
   </action>

   <action name="check_all">
      <filter id="is_recordflux"/>
      <shell lang="python" show-command="false">
         " ".join([s.name()
                   for s in GPS.current_context().project().sources()
                   if s.language() == "recordflux"])
      </shell>
      <external>rflx check %1"</external>
   </action>

   <action name="generate">
      <filter id="is_recordflux"/>
      <external>rflx generate -d %o %F</external>
   </action>

   <action name="generate_all">
      <filter id="is_recordflux"/>
      <shell lang="python" show-command="false">
         " ".join([s.name()
                   for s in GPS.current_context().project().sources()
                   if s.language() == "recordflux"])
      </shell>
      <external>rflx generate -d %o %1"</external>
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

@hook('gps_started')
def __on_gps_started():
    GPS.FileTemplate.register(
        alias_name="rflx_package",
        label="RecordFlux specification",
        unit_param="name",
        language="RecordFlux",
        is_impl=False)
