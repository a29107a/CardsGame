-module(pb_codec).

-export([encode/1, decode/1]).

encode(GoogleMessageTuple) ->
  MessageName = erlang:element(1,GoogleMessageTuple),
  {ErlFile,ProtoMessageNum} = proto_message_num_mapper:get(MessageName),
  MessageBinary = ErlFile:encode_msg(GoogleMessageTuple),
  {Compressed, CompressedBinary}  =
    case erlang:byte_size(MessageBinary) of
      GreaterThan2048Bytes when GreaterThan2048Bytes >= 2048 ->
        {1 ,zlib:gzip(MessageBinary)};
      _ ->
        {0, MessageBinary}
    end,
  <<
    Compressed:1/big-unsigned-integer-unit:8,
    ProtoMessageNum:1/big-unsigned-integer-unit:16,
    CompressedBinary/binary
    >>.

decode(MessageBinary) ->
  <<
    Compressed:1/big-unsigned-integer-unit:8,
    ProtoMessageNum:1/big-unsigned-integer-unit:16,
    CompressedBinary/binary
  >> = MessageBinary,
  GoogleMessageBinary =
    case Compressed of
      0 ->
        CompressedBinary;
      1 ->
        zlib:gunzip(CompressedBinary)
    end,
  {ErlFile,MessageName} = proto_message_num_mapper:get(ProtoMessageNum),
  GoogleMessage = ErlFile:decode_msg(GoogleMessageBinary,MessageName),
  GoogleMessage.
