using System.Collections.Generic;

using Newtonsoft.Json;

namespace OilGas.Data.RRC.Texas
{
    /// <summary>
    /// https://rrcsearch3.neubus.com/esd3-rrc/index.php?_module_=esd&_action_=keysearch&profile=15#
    /// </summary>
    public class NeubusSearch
    {
        [JsonProperty("Neusearch", Required = Required.Always)]
        public Neusearch Neusearch { get; set; }
    }

    public class Neusearch
    {
        [JsonProperty("profile", Required = Required.Always)]
        public long Profile { get; set; }

        [JsonProperty("Searchitems", Required = Required.Always)]
        public SearchItems SearchItems { get; set; }

        [JsonProperty("includeName", Required = Required.Always)]
        public string IncludeName { get; set; }

        [JsonProperty("excludeName", Required = Required.Always)]
        public string ExcludeName { get; set; }

        [JsonProperty("excludeValue", Required = Required.Always)]
        public string ExcludeValue { get; set; }

        [JsonProperty("recordFromDate", Required = Required.Always)]
        public string RecordFromDate { get; set; }

        [JsonProperty("recordToDate", Required = Required.Always)]
        public string RecordToDate { get; set; }

        [JsonProperty("page", Required = Required.Always)]
        public long Page { get; set; }

        [JsonProperty("pageSize", Required = Required.Always)]
        public long PageSize { get; set; }

        [JsonProperty("strict", Required = Required.Always)]
        [JsonConverter(typeof(ParseStringConverter))]
        public bool Strict { get; set; }

        [JsonProperty("saveSearch", Required = Required.Always)]
        [JsonConverter(typeof(ParseStringConverter))]
        public bool SaveSearch { get; set; }
    }

    public class SearchItems
    {
        [JsonProperty("item", Required = Required.Always)]
        public List<SearchItem> Item { get; set; }
    }

    public class SearchItem
    {
        [JsonProperty("key", Required = Required.Always)]
        public string Key { get; set; }

        [JsonProperty("value", Required = Required.Always)]
        public string Value { get; set; }
    }
}
