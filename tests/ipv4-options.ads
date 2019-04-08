with Message_Sequence;
with IPv4.Option;

package IPv4.Options is new Message_Sequence (Option.Label, Option.Is_Contained, Option.Is_Valid, Option.Message_Length);
