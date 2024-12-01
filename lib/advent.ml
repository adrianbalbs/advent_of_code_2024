let read_lines file =
  In_channel.with_open_bin file In_channel.input_all
  |> String.split_on_char '\n'
