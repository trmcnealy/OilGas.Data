using System;
using System.Collections.Generic;
using System.Globalization;
using System.Runtime.CompilerServices;

using Newtonsoft.Json;
using Newtonsoft.Json.Converters;

namespace OilGas.Data.RRC.Texas
{
    public class GisQuery
    {
        [JsonProperty("displayFieldName", NullValueHandling = NullValueHandling.Ignore)]
        public string DisplayFieldName { get; set; }

        [JsonProperty("fieldAliases", NullValueHandling = NullValueHandling.Ignore)]
        public GisFieldAliases FieldAliases { get; set; }

        [JsonProperty("geometryType", NullValueHandling = NullValueHandling.Ignore)]
        public string GeometryType { get; set; }

        [JsonProperty("spatialReference", NullValueHandling = NullValueHandling.Ignore)]
        public GisSpatialReference SpatialReference { get; set; }

        [JsonProperty("fields", NullValueHandling = NullValueHandling.Ignore)]
        public List<GisField> Fields { get; set; }

        [JsonProperty("features", NullValueHandling = NullValueHandling.Ignore)]
        public List<GisFeature> Features { get; set; }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public string ToJson() => JsonConvert.SerializeObject(this, Converter.Settings);

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static GisQuery FromJson(string json) => JsonConvert.DeserializeObject<GisQuery>(json, Converter.Settings);

        public class GisAttributes
        {
            [JsonProperty("UNIQID", NullValueHandling = NullValueHandling.Ignore)]
            public long Uniqid { get; set; }

            [JsonProperty("API", NullValueHandling = NullValueHandling.Ignore)]
            [JsonConverter(typeof(ParseStringConverter))]
            public long Api { get; set; }

            [JsonProperty("GIS_API5", NullValueHandling = NullValueHandling.Ignore)]
            [JsonConverter(typeof(ParseStringConverter))]
            public long GisApi5 { get; set; }

            [JsonProperty("GIS_WELL_NUMBER", NullValueHandling = NullValueHandling.Ignore)]
            public string GisWellNumber { get; set; }

            [JsonProperty("SYMNUM", NullValueHandling = NullValueHandling.Ignore)]
            public long Symnum { get; set; }

            [JsonProperty("GIS_SYMBOL_DESCRIPTION", NullValueHandling = NullValueHandling.Ignore)]
            public string GisSymbolDescription { get; set; }

            [JsonProperty("RELIAB", NullValueHandling = NullValueHandling.Ignore)]
            [JsonConverter(typeof(ParseStringConverter))]
            public long Reliab { get; set; }

            [JsonProperty("GIS_LOCATION_SOURCE", NullValueHandling = NullValueHandling.Ignore)]
            public string GisLocationSource { get; set; }

            [JsonProperty("GIS_LAT27", NullValueHandling = NullValueHandling.Ignore)]
            public double GisLat27 { get; set; }

            [JsonProperty("GIS_LONG27", NullValueHandling = NullValueHandling.Ignore)]
            public double GisLong27 { get; set; }

            [JsonProperty("GIS_LAT83", NullValueHandling = NullValueHandling.Ignore)]
            public double GisLat83 { get; set; }

            [JsonProperty("GIS_LONG83", NullValueHandling = NullValueHandling.Ignore)]
            public double GisLong83 { get; set; }

            [JsonProperty("OBJECTID", NullValueHandling = NullValueHandling.Ignore)]
            public long Objectid { get; set; }
        }

        public class GisFeature
        {
            [JsonProperty("attributes", NullValueHandling = NullValueHandling.Ignore)]
            public GisAttributes Attributes { get; set; }

            [JsonProperty("geometry", NullValueHandling = NullValueHandling.Ignore)]
            public GisGeometry Geometry { get; set; }
        }

        public class GisGeometry
        {
            [JsonProperty("x", NullValueHandling = NullValueHandling.Ignore)]
            public double X { get; set; }

            [JsonProperty("y", NullValueHandling = NullValueHandling.Ignore)]
            public double Y { get; set; }
        }

        public class GisFieldAliases
        {
            [JsonProperty("UNIQID", NullValueHandling = NullValueHandling.Ignore)]
            public string Uniqid { get; set; }

            [JsonProperty("API", NullValueHandling = NullValueHandling.Ignore)]
            public string Api { get; set; }

            [JsonProperty("GIS_API5", NullValueHandling = NullValueHandling.Ignore)]
            public string GisApi5 { get; set; }

            [JsonProperty("GIS_WELL_NUMBER", NullValueHandling = NullValueHandling.Ignore)]
            public string GisWellNumber { get; set; }

            [JsonProperty("SYMNUM", NullValueHandling = NullValueHandling.Ignore)]
            public string Symnum { get; set; }

            [JsonProperty("GIS_SYMBOL_DESCRIPTION", NullValueHandling = NullValueHandling.Ignore)]
            public string GisSymbolDescription { get; set; }

            [JsonProperty("RELIAB", NullValueHandling = NullValueHandling.Ignore)]
            public string Reliab { get; set; }

            [JsonProperty("GIS_LOCATION_SOURCE", NullValueHandling = NullValueHandling.Ignore)]
            public string GisLocationSource { get; set; }

            [JsonProperty("GIS_LAT27", NullValueHandling = NullValueHandling.Ignore)]
            public string GisLat27 { get; set; }

            [JsonProperty("GIS_LONG27", NullValueHandling = NullValueHandling.Ignore)]
            public string GisLong27 { get; set; }

            [JsonProperty("GIS_LAT83", NullValueHandling = NullValueHandling.Ignore)]
            public string GisLat83 { get; set; }

            [JsonProperty("GIS_LONG83", NullValueHandling = NullValueHandling.Ignore)]
            public string GisLong83 { get; set; }

            [JsonProperty("OBJECTID", NullValueHandling = NullValueHandling.Ignore)]
            public string Objectid { get; set; }
        }

        public class GisField
        {
            [JsonProperty("name", NullValueHandling = NullValueHandling.Ignore)]
            public string Name { get; set; }

            [JsonProperty("type", NullValueHandling = NullValueHandling.Ignore)]
            public string Type { get; set; }

            [JsonProperty("alias", NullValueHandling = NullValueHandling.Ignore)]
            public string Alias { get; set; }

            [JsonProperty("length", Required = Required.DisallowNull, NullValueHandling = NullValueHandling.Ignore)]
            public long? Length { get; set; }
        }

        public class GisSpatialReference
        {
            [JsonProperty("wkid", NullValueHandling = NullValueHandling.Ignore)]
            public long Wkid { get; set; }

            [JsonProperty("latestWkid", NullValueHandling = NullValueHandling.Ignore)]
            public long LatestWkid { get; set; }
        }

        internal static class Converter
        {
            public static readonly JsonSerializerSettings Settings = new JsonSerializerSettings
            {
                MetadataPropertyHandling = MetadataPropertyHandling.Ignore,
                DateParseHandling        = DateParseHandling.None,
                Converters =
                {
                    new IsoDateTimeConverter
                    {
                        DateTimeStyles = DateTimeStyles.AssumeUniversal
                    }
                }
            };
        }

        internal class ParseStringConverter : JsonConverter
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public override bool CanConvert(Type t) => t == typeof(long) || t == typeof(long?);

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public override object ReadJson(JsonReader     reader,
                                            Type           t,
                                            object         existingValue,
                                            JsonSerializer serializer)
            {
                if(reader.TokenType == JsonToken.Null)
                    return null;

                string  value = serializer.Deserialize<string>(reader);

                if(long.TryParse(value, out long l))
                {
                    return l;
                }

                throw new Exception("Cannot unmarshal type long");
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public override void WriteJson(JsonWriter     writer,
                                           object         untypedValue,
                                           JsonSerializer serializer)
            {
                if(untypedValue is null)
                {
                    serializer.Serialize(writer, null);

                    return;
                }

                long value = (long)untypedValue;
                serializer.Serialize(writer, value.ToString());
            }

            public static readonly ParseStringConverter Singleton = new ParseStringConverter();
        }
    }
}
