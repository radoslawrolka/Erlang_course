defmodule Pollutiondb.Reading do
  use Ecto.Schema
  require Ecto.Query

  schema "readings" do
    field :datetime, :utc_datetime
    field :type, :string
    field :value, :float
    belongs_to :station, Pollutiondb.Station
  end

  def add(station, type, value, datetime) do
    %Pollutiondb.Reading{
      station: station, type: type, value: value, datetime: datetime}
    |> Pollutiondb.Repo.insert()
  end

  def add_now(station, type, value) do
    %Pollutiondb.Reading{
      station: station, type: type, value: value,
      datetime: DateTime.utc_now |> DateTime.truncate(:second)}
    |> Pollutiondb.Repo.insert()
  end

  def getAll() do
    Pollutiondb.Repo.all(Pollutiondb.Reading)
    |> Pollutiondb.Repo.preload(:station)
  end

  def find_by_date(date) do
    minDateTime = DateTime.new!(date, ~T[00:00:00])
    maxDateTime = DateTime.add(minDateTime, 24*60*60, :second)
    Ecto.Query.from(r in Pollutiondb.Reading,
      where: ^minDateTime <= r.datetime,
      where: r.datetime <= ^maxDateTime)
    |> Pollutiondb.Repo.all
  end

end