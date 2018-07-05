pragma SPARK_Mode (On);

with Types; use Types;
pragma Elaborate_All (Types);

package Ethernet_Dissector is

    function Match(Buffer : in Bytes) return Natural
        with
            Post => Match'Result = Buffer'Length or Match'Result = 0;

end Ethernet_Dissector;
