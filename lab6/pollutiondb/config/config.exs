import Config

config :pollutiondb, ecto_repos: [Pollutiondb.Repo]
config :pollutiondb, Pollutiondb.Repo, database: "database/pollutiondb.db",
  username: "user",
  password: "pass",
  hostname: "localhost"
