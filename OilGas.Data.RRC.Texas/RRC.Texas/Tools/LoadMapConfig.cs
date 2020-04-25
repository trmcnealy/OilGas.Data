using System;
using System.Collections.Generic;
using System.Globalization;
using System.Runtime.Serialization;

using Newtonsoft.Json;
using Newtonsoft.Json.Converters;

namespace OilGas.Data.RRC.Texas
{
    public sealed class MapConfig
    {
        [JsonProperty("operationalLayerUrl", Required = Required.Always)]
        public Uri OperationalLayerUrl { get; set; }

        [JsonProperty("baseMapUrl", Required = Required.Always)]
        public Uri BaseMapUrl { get; set; }

        [JsonProperty("baseMapUrlForSurvey", Required = Required.Always)]
        public Uri BaseMapUrlForSurvey { get; set; }

        [JsonProperty("baseMapUrlForSurveyLabels", Required = Required.Always)]
        public Uri BaseMapUrlForSurveyLabels { get; set; }

        [JsonProperty("geometryserviceurl", Required = Required.Always)]
        public Uri Geometryserviceurl { get; set; }

        [JsonProperty("printtask", Required = Required.Always)]
        public Uri Printtask { get; set; }

        [JsonProperty("placefinder", Required = Required.Always)]
        public Placefinder Placefinder { get; set; }

        [JsonProperty("customlogo", Required = Required.Always)]
        public Customlogo Customlogo { get; set; }

        [JsonProperty("leaseIdSearchUrl", Required = Required.Always)]
        public string LeaseIdSearchUrl { get; set; }

        [JsonProperty("wellboreUrlTemplate", Required = Required.Always)]
        public string WellboreUrlTemplate { get; set; }

        [JsonProperty("completionUrlTemplate", Required = Required.Always)]
        public string CompletionUrlTemplate { get; set; }

        [JsonProperty("pdqUrlTemplate", Required = Required.Always)]
        public string PdqUrlTemplate { get; set; }

        [JsonProperty("pdqPath", Required = Required.Always)]
        public Uri PdqPath { get; set; }

        [JsonProperty("specialtyUrlTemplate", Required = Required.Always)]
        public string SpecialtyUrlTemplate { get; set; }

        [JsonProperty("geomSrvUrl", Required = Required.Always)]
        public string GeomSrvUrl { get; set; }

        [JsonProperty("detailUrlWellsBase", Required = Required.Always)]
        public Uri DetailUrlWellsBase { get; set; }

        [JsonProperty("detailUrlWells1", Required = Required.Always)]
        public string DetailUrlWells1 { get; set; }

        [JsonProperty("detailUrlWells2", Required = Required.Always)]
        public string DetailUrlWells2 { get; set; }

        [JsonProperty("detailUrlWells3", Required = Required.Always)]
        public string DetailUrlWells3 { get; set; }

        [JsonProperty("neubusUrlBase", Required = Required.Always)]
        public Uri NeubusUrlBase { get; set; }

        [JsonProperty("urlFragmentFunctionLogs", Required = Required.Always)]
        public string UrlFragmentFunctionLogs { get; set; }

        [JsonProperty("urlFragmentFunctionLogsforDisposalPermit", Required = Required.Always)]
        public string UrlFragmentFunctionLogsforDisposalPermit { get; set; }

        [JsonProperty("urlFragmentFunctionImage", Required = Required.Always)]
        public string UrlFragmentFunctionImage { get; set; }

        [JsonProperty("urlFragmentApiNo", Required = Required.Always)]
        public string UrlFragmentApiNo { get; set; }

        [JsonProperty("urlFragmentApiNoforDisposalPermit", Required = Required.Always)]
        public string UrlFragmentApiNoforDisposalPermit { get; set; }

        [JsonProperty("urlFragmentDistrict", Required = Required.Always)]
        public string UrlFragmentDistrict { get; set; }

        [JsonProperty("urlFragmentLeaseId", Required = Required.Always)]
        public string UrlFragmentLeaseId { get; set; }

        [JsonProperty("orphanProcedurePdf", Required = Required.Always)]
        public Uri OrphanProcedurePdf { get; set; }

        [JsonProperty("drillingPermitUrlBase", Required = Required.Always)]
        public Uri DrillingPermitUrlBase { get; set; }

        [JsonProperty("drillingPermitUrl1", Required = Required.Always)]
        public string DrillingPermitUrl1 { get; set; }

        [JsonProperty("wasteDisposalDistictUrl", Required = Required.Always)]
        public Uri WasteDisposalDistictUrl { get; set; }

        [JsonProperty("rrcDistrictContactUrl", Required = Required.Always)]
        public Uri RrcDistrictContactUrl { get; set; }

        [JsonProperty("layerIdWells", Required = Required.Always)]
        public long LayerIdWells { get; set; }

        [JsonProperty("layerIdHighlightOrphans", Required = Required.Always)]
        public long LayerIdHighlightOrphans { get; set; }

        [JsonProperty("layerIdHighlightUICCS", Required = Required.Always)]
        public long LayerIdHighlightUiccs { get; set; }

        [JsonProperty("layerIdHighlightUICID", Required = Required.Always)]
        public long LayerIdHighlightUicid { get; set; }

        [JsonProperty("layerIdHighlightHCTS15K", Required = Required.Always)]
        public long LayerIdHighlightHcts15K { get; set; }

        [JsonProperty("layerIdHighlightHCTS", Required = Required.Always)]
        public long LayerIdHighlightHcts { get; set; }

        [JsonProperty("layerIdHighlightH13", Required = Required.Always)]
        public long LayerIdHighlightH13 { get; set; }

        [JsonProperty("layerIdHighlightLogs", Required = Required.Always)]
        public long LayerIdHighlightLogs { get; set; }

        [JsonProperty("layerIdSurfaceWells", Required = Required.Always)]
        public long LayerIdSurfaceWells { get; set; }

        [JsonProperty("layerIdWellSticks", Required = Required.Always)]
        public long LayerIdWellSticks { get; set; }

        [JsonProperty("layerIdLpgSites", Required = Required.Always)]
        public long LayerIdLpgSites { get; set; }

        [JsonProperty("layerIdPipelinesLarge", Required = Required.Always)]
        public long LayerIdPipelinesLarge { get; set; }

        [JsonProperty("layerIdPipelinesSmall", Required = Required.Always)]
        public long LayerIdPipelinesSmall { get; set; }

        [JsonProperty("layerIdBay", Required = Required.Always)]
        public long LayerIdBay { get; set; }

        [JsonProperty("layerIdOffShore", Required = Required.Always)]
        public long LayerIdOffShore { get; set; }

        [JsonProperty("layerIdOffshoreTracts", Required = Required.Always)]
        public long LayerIdOffshoreTracts { get; set; }

        [JsonProperty("layerIdRivers", Required = Required.Always)]
        public long LayerIdRivers { get; set; }

        [JsonProperty("layerIdSubdivisionLabels", Required = Required.Always)]
        public long LayerIdSubdivisionLabels { get; set; }

        [JsonProperty("layerIdSubdivisions", Required = Required.Always)]
        public long LayerIdSubdivisions { get; set; }

        [JsonProperty("layerIdRailroads", Required = Required.Always)]
        public long LayerIdRailroads { get; set; }

        [JsonProperty("layerIdSurveyAbstractLabels", Required = Required.Always)]
        public long LayerIdSurveyAbstractLabels { get; set; }

        [JsonProperty("layerIdSurveyLabels", Required = Required.Always)]
        public long LayerIdSurveyLabels { get; set; }

        [JsonProperty("layerIdSurveys", Required = Required.Always)]
        public long LayerIdSurveys { get; set; }

        [JsonProperty("layerIdQuads", Required = Required.Always)]
        public long LayerIdQuads { get; set; }

        [JsonProperty("layerIdAlertAreas", Required = Required.Always)]
        public long LayerIdAlertAreas { get; set; }

        [JsonProperty("layerIdWaterBodies", Required = Required.Always)]
        public long LayerIdWaterBodies { get; set; }

        [JsonProperty("layerIdCityLimits", Required = Required.Always)]
        public long LayerIdCityLimits { get; set; }

        [JsonProperty("layerIdCounty", Required = Required.Always)]
        public long LayerIdCounty { get; set; }

        [JsonProperty("layerIdDistrictOffices", Required = Required.Always)]
        public long LayerIdDistrictOffices { get; set; }

        [JsonProperty("layerIdOGDistrict", Required = Required.Always)]
        public long LayerIdOgDistrict { get; set; }

        [JsonProperty("layerIdWaterLabels", Required = Required.Always)]
        public long LayerIdWaterLabels { get; set; }

        [JsonProperty("layerIdOCPSites", Required = Required.Always)]
        public long LayerIdOcpSites { get; set; }

        [JsonProperty("layerIdVCPSites", Required = Required.Always)]
        public long LayerIdVcpSites { get; set; }

        [JsonProperty("layerIdBRPSites", Required = Required.Always)]
        public long LayerIdBrpSites { get; set; }

        [JsonProperty("layerIdWasteDisposalFacilities", Required = Required.Always)]
        public long LayerIdWasteDisposalFacilities { get; set; }

        [JsonProperty("layerIdDischargePermit", Required = Required.Always)]
        public long LayerIdDischargePermit { get; set; }

        [JsonProperty("layerIdAedDistrict", Required = Required.Always)]
        public long LayerIdAedDistrict { get; set; }

        [JsonProperty("layerIdPsdDistrict", Required = Required.Always)]
        public long LayerIdPsdDistrict { get; set; }

        [JsonProperty("queryLayers", Required = Required.Always)]
        public List<QueryLayer> QueryLayers { get; set; }

        [JsonProperty("longitudeList", Required = Required.Always)]
        public List<string> LongitudeList { get; set; }

        [JsonProperty("numericAttrList", Required = Required.Always)]
        public List<string> NumericAttrList { get; set; }

        [JsonProperty("pipelineQueryFields", Required = Required.Always)]
        public List<string> PipelineQueryFields { get; set; }

        [JsonProperty("cwdQueryFields", Required = Required.Always)]
        public List<string> CwdQueryFields { get; set; }

        [JsonProperty("dischargePermitQueryFields", Required = Required.Always)]
        public List<string> DischargePermitQueryFields { get; set; }

        public static MapConfig FromJson(string json) => JsonConvert.DeserializeObject<MapConfig>(json, Converter.Settings);

        public string ToJson() => JsonConvert.SerializeObject(this, Converter.Settings);

        private const string defaultJson = "{\n" +
                                           "	\"operationalLayerUrl\": \"https://gis.rrc.texas.gov/server/rest/services/rrc_public/RRC_Public_Viewer_Srvs/MapServer\",\n" +
                                           "	\"baseMapUrl\": \"https://services.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer\",\n" +
                                           "	\"baseMapUrlForSurvey\": \"https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer\",\n" +
                                           "	\"baseMapUrlForSurveyLabels\": \"https://services.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/MapServer\",\n" +
                                           "	\"geometryserviceurl\": \"https://gis.rrc.texas.gov/server/rest/services/Utilities/Geometry/GeometryServer\",\n" +
                                           "	\"printtask\": \"https://gis.rrc.texas.gov/server/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task\",\n" +
                                           "	\n" +
                                           "	\"placefinder\": {\n" +
                                           "		\"url\": \"https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/findAddressCandidates?\",\n" +
                                           "		\"singlelinefieldname\": \"SingleLine\"\n" +
                                           "	},\n" +
                                           "\n" +
                                           "	\"customlogo\": {\n" +
                                           "		\"image\": \"GISViewer/images/rrc-logo-lg.gif\",\n" +
                                           "		\"link\": \"https://www.rrc.texas.gov/\"\n" +
                                           "	},\n" +
                                           "\n" +
                                           "	\"leaseIdSearchUrl\": \"GISViewer/proxy/proxy.ashx?https://gisutil106p.rrc.texas.gov/GISWebServices/api/OGWellExtract?leaseno=\",\n" +
                                           "	\"wellboreUrlTemplate\": \"GISViewer/proxy/proxy.ashx?https://gisutil106p.rrc.texas.gov/GISWebServices/api/Wellbore?apiNumber=\",\n" +
                                           "	\"completionUrlTemplate\": \"GISViewer/proxy/proxy.ashx?https://gisutil106p.rrc.texas.gov/GISWebServices/api/Completion?apiNumber=\",\n" +
                                           "	\"pdqUrlTemplate\": \"GISViewer/proxy/proxy.ashx?https://gisutil106p.rrc.texas.gov/GISWebServices/api/PdqProdCycle\",\n" +
                                           "	\"pdqPath\": \"http://webapps2.rrc.texas.gov/EWA/specificLeaseQueryAction.do?tab=init&viewType=prodAndTotalDisp&methodToCall=fromGisViewer&pdqSearchArgs.paramValue=|2=\", \n" +
                                           "	\"specialtyUrlTemplate\": \"GISViewer/proxy/proxy.ashx?https://gisutil106p.rrc.texas.gov/GISWebServices/api/WellboreApilut?leaseNo=\",\n" +
                                           "	\"geomSrvUrl\": \"GISViewer/proxy/proxy.ashx?https://gis.rrc.texas.gov/server/rest/services/Utilities/Geometry/GeometryServer/project?inSR=\",\n" +
                                           "	\"detailUrlWellsBase\": \"http://webapps2.rrc.texas.gov/EWA/leaseDetailAction.do?searchType=apiNo&selTab=2048&apiNo=\",\n" +
                                           "	\"detailUrlWells1\": \"&distCode=\",\n" +
                                           "	\"detailUrlWells2\": \"&leaseNo=\",\n" +
                                           "	\"detailUrlWells3\": \"&methodToCall=displayLeaseDetail\",\n" +
                                           "	\"neubusUrlBase\": \"https://rrcsearch3.neubus.com/esd3-rrc/api.php?\",\n" +
                                           "	\"urlFragmentFunctionLogs\": \"function=GetWellLogs\",\n" +
                                           "	\"urlFragmentFunctionLogsforDisposalPermit\": \"function=GetWellLogs\",\n" +
                                           "	\"urlFragmentFunctionImage\": \"function=GetImage\",\n" +
                                           "	\"urlFragmentApiNo\": \"&api_no=\",\n" +
                                           "	\"urlFragmentApiNoforDisposalPermit\": \"&api_no=\",\n" +
                                           "	\"urlFragmentDistrict\": \"&district=\",\n" +
                                           "	\"urlFragmentLeaseId\": \"&lease_id=\",\n" +
                                           "	\"orphanProcedurePdf\": \"https://www.rrc.texas.gov/media/8874/orphanwelltakeoverprocedure.pdf\",\n" +
                                           "	\"drillingPermitUrlBase\": \"http://webapps.rrc.texas.gov/DP/publicQuerySearchAction.do?countyCode=\",\n" +
                                           "	\"drillingPermitUrl1\": \"&apiSeqNo=\",\n" +
                                           "	\"wasteDisposalDistictUrl\": \"https://rrc.texas.gov/oil-gas/applications-and-permits/environmental-permit-types-information/commercial-surface-waste-facilities/commercial-recyclingdisposal-permits-list/district-\",\n" +
                                           "	\"rrcDistrictContactUrl\": \"https://www.rrc.state.tx.us/about-us/organization-activities/rrc-locations/#OilGasLocations\",\n" +
                                           "\n" +
                                           "	\"layerIdWells\": 1,\n" +
                                           "	\"layerIdHighlightOrphans\": 2,\n" +
                                           "	\"layerIdHighlightUICCS\": 3,\n" +
                                           "	\"layerIdHighlightUICID\": 4,\n" +
                                           "	\"layerIdHighlightHCTS15K\": 5,\n" +
                                           "	\"layerIdHighlightHCTS\": 6,\n" +
                                           "	\"layerIdHighlightH13\": 7,\n" +
                                           "	\"layerIdHighlightLogs\": 8,\n" +
                                           "	\"layerIdSurfaceWells\": 9,\n" +
                                           "	\"layerIdWellSticks\": 10,\n" +
                                           "	\"layerIdLpgSites\": 11,\n" +
                                           "	\"layerIdPipelinesLarge\": 13,\n" +
                                           "	\"layerIdPipelinesSmall\": 14,\n" +
                                           "	\"layerIdBay\": 15,\n" +
                                           "	\"layerIdOffShore\": 16,\n" +
                                           "	\"layerIdOffshoreTracts\": 17,\n" +
                                           "	\"layerIdRivers\": 18,\n" +
                                           "	\"layerIdSubdivisionLabels\": 19,\n" +
                                           "	\"layerIdSubdivisions\": 20,\n" +
                                           "	\"layerIdRailroads\": 21,\n" +
                                           "	\"layerIdSurveyAbstractLabels\": 22,\n" +
                                           "	\"layerIdSurveyLabels\": 23,\n" +
                                           "	\"layerIdSurveys\": 24,\n" +
                                           "	\"layerIdQuads\": 25,\n" +
                                           "	\"layerIdAlertAreas\": 26,\n" +
                                           "	\"layerIdWaterBodies\": 27,\n" +
                                           "	\"layerIdCityLimits\": 28,\n" +
                                           "	\"layerIdCounty\": 29,\n" +
                                           "	\"layerIdDistrictOffices\": 30,\n" +
                                           "	\"layerIdOGDistrict\": 31,\n" +
                                           "	\"layerIdWaterLabels\": 32,\n" +
                                           "	\"layerIdOCPSites\": 33,\n" +
                                           "	\"layerIdVCPSites\": 34,\n" +
                                           "	\"layerIdBRPSites\": 35,\n" +
                                           "	\"layerIdWasteDisposalFacilities\": 36,\n" +
                                           "	\"layerIdDischargePermit\": 37,\n" +
                                           "	\"layerIdAedDistrict\": 38,\n" +
                                           "	\"layerIdPsdDistrict\": 39,\n" +
                                           "\n" +
                                           "	\"queryLayers\": [{\n" +
                                           "		\"paramName\": \"api\",\n" +
                                           "		\"restId\": \"1\",\n" +
                                           "		\"fldName\": \"API\"\n" +
                                           "	},\n" +
                                           "		{\n" +
                                           "			\"paramName\": \"surveys\",\n" +
                                           "			\"restId\": \"25\",\n" +
                                           "			\"fldName\": \"ABSTRACT_NUMBER\"\n" +
                                           "		},\n" +
                                           "\n" +
                                           "		{\n" +
                                           "			\"paramName\": \"pipelinesLarge\",\n" +
                                           "			\"restId\": \"13\",\n" +
                                           "			\"fldName\": \"OBJECTID\"\n" +
                                           "		},\n" +
                                           "		{\n" +
                                           "			\"paramName\": \"commodity\",\n" +
                                           "			\"restId\": \"37\",\n" +
                                           "			\"fldName\": \"COMMODITY_ID\"\n" +
                                           "		}\n" +
                                           "	],\n" +
                                           "	\"longitudeList\": [\"Long27\", \"Long83\"],\n" +
                                           "	\"numericAttrList\": [\"API\", \"Well_Number\", \"Well_No\", \"Long27\", \"Long83\", \"Last_Permit_Issued\", \"District\", \"District_Name\", \"LeaseID\", \"Operator_Number\"],\n" +
                                           "	\"pipelineQueryFields\": [\"COMMODITY\", \"SYSTEM_TYPE\", \"INTERSTATE\", \"STATUS\", \"OPERATOR\", \"P5_NUM\", \"T4PERMIT\", \"DIAMETER\", \"SYSTEM_NAME\", \"SUBSYSTEM_NAME\", \"OBJECTID\", \"COUNTY_FIPS\", \"COUNTY_NAME\", \"CONTACT_PHONE_NUMBER\"],\n" +
                                           "	\"cwdQueryFields\": [\"OBJECTID\", \"PERMIT_NO\", \"LEASE_OR_FACILITY_NAME\", \"OPERATOR_NAME\", \"PERMIT_TYPE\", \"COUNTY\"],\n" +
                                           "	\"dischargePermitQueryFields\": [\"OBJECTID\", \"PERMIT_NO\", \"LEASE_OR_FACILITY_NAME\", \"OPERATOR_NAME\", \"PERMIT_TYPE\", \"COUNTY\", \"DISCHARGE_TYPE\", \"PERMIT_EXPIRATION\", \"NPDES_PERMIT_NO\", \"DISCHARGE_VOLUME\"]\n" +
                                           "}";

        public static MapConfig Load()
        {
            return FromJson(defaultJson);
        }
    }

    public sealed class Customlogo
    {
        [JsonProperty("image", Required = Required.Always)]
        public string Image { get; set; }

        [JsonProperty("link", Required = Required.Always)]
        public Uri Link { get; set; }
    }

    public sealed class Placefinder
    {
        [JsonProperty("url", Required = Required.Always)]
        public Uri Url { get; set; }

        [JsonProperty("singlelinefieldname", Required = Required.Always)]
        public string Singlelinefieldname { get; set; }
    }

    public sealed class QueryLayer
    {
        [JsonProperty("paramName", Required = Required.Always)]
        public string ParamName { get; set; }

        [JsonProperty("restId", Required = Required.Always)]
        [JsonConverter(typeof(ParseStringConverter))]
        public long RestId { get; set; }

        [JsonProperty("fldName", Required = Required.Always)]
        public string FldName { get; set; }
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
            },
        };
    }

    internal sealed class ParseStringConverter : JsonConverter
    {
        public override bool CanConvert(Type t) => t == typeof(long) || t == typeof(long?);

        public override object ReadJson(JsonReader     reader,
                                        Type           t,
                                        object         existingValue,
                                        JsonSerializer serializer)
        {
            if(reader.TokenType == JsonToken.Null)
                return null;

            var  value = serializer.Deserialize<string>(reader);
            long l;

            if(Int64.TryParse(value, out l))
            {
                return l;
            }

            throw new Exception("Cannot unmarshal type long");
        }

        public override void WriteJson(JsonWriter     writer,
                                       object         untypedValue,
                                       JsonSerializer serializer)
        {
            if(untypedValue == null)
            {
                serializer.Serialize(writer, null);

                return;
            }

            var value = (long)untypedValue;
            serializer.Serialize(writer, value.ToString());

            return;
        }

        public static readonly ParseStringConverter Singleton = new ParseStringConverter();
    }
}
