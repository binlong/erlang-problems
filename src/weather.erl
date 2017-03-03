-module(weather).
-export([forecast/1, extract_forecast_weather/1]).

forecast(Cities) ->
	Self = self(),
	Pids = [spawn_link(fun() -> Self ! {self(), extract_forecast_weather(weather_api:get_weather(City))} end) || City <- Cities],
	[receive {Pid, Forecast} -> Forecast end || Pid <- Pids].


extract_forecast_weather(Weather) ->
	{_, _, Forecast} = Weather,
	{_, _, Forecast_Weather} = Forecast,
	Forecast_Weather.