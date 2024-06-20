defmodule Pollutiondb.DataLoader do
  @path "./AirlyData-ALL-50k.csv"

  def read_data() do
    @path
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&process_line/1)
  end

  defp process_line(line) do
    [dateTime, pollutionType, pollutionLevel, stationId, stationName, location] =
      String.split(line, ";")

    if Pollutiondb.Station.findByName(stationName) == nil do
      IO.puts("Adding station: #{stationName}")
      [lon, lat] = String.split(location, ",")
      Pollutiondb.Station.add(%Pollutiondb.Station{name: stationName, lon: String.to_float(lon), lat: String.to_float(lat)})
    end

    Pollutiondb.Reading.add(
      Pollutiondb.Station.findByName(stationName),
      pollutionType,
      String.to_float(pollutionLevel),
      to_utc_datetime(dateTime))
  end

  defp to_utc_datetime(string_tuple) do
    {{yyyy, mm, dd}, {hh, min, ss}} = parse_string_tuple(string_tuple)
    DateTime.new!(Date.new!(yyyy, mm, dd), Time.new!(hh, min, ss))
  end

  defp parse_string_tuple(datetime) do
    [date_part, time_part] = String.split(datetime, "T")
    [date_year, date_month, date_day] = String.split(date_part, "-")

    [time_hour, time_minute, time_second] =
      String.split(Enum.at(String.split(time_part, "."), 0), ":")

    {{String.to_integer(date_year), String.to_integer(date_month), String.to_integer(date_day)},
      {String.to_integer(time_hour), String.to_integer(time_minute),
        String.to_integer(time_second)}}
  end

end