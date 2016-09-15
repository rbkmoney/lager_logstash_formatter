# lager_logstash_formatter

Lager форматер, следующий [RBKmoney требованиям по форматированию логов](https://github.com/rbkmoney/coredocs/blob/ft/MSPF-79/platform-logging/docs/design/ms/platform/overview.md).

За основу взят [talentdeficit/lager2json](https://github.com/talentdeficit/lager2json), использующий [jsx](https://github.com/talentdeficit/jsx) для создания JSON.

## Config options

* `ct_backend` - когда установлена в `true`, _formatter_ возвращает `io list`, совместимый с логированием в `CT`.

