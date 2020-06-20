using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

using AngleSharp.Text;

namespace OilGas.Data.RRC.Texas
{
    public static class TapeRecordClassBuilder
    {
        private const string _level = "(?<level>[^\\s]*)\\s*";
        private const string _name  = "(?<name>[^\\s]*)\\s*";

        private const string _type_1 = "PIC\\s*(?<type>[^\\.|\\s|\\(]*)";
        private const string _type_2 = "COMP-3\\s*PIC\\s*(?<type>[^\\.|\\s|\\(]*)";

        private const string _type = "((?=P)" + _type_1 + "|" + _type_2 + ")";

        private const string _size_1 = "([\\.]*)\\s*";
        private const string _size_2 = "(?<v_type>[^\\(]*)\\((?<v_size>[^\\)]*)\\)([\\.]*)\\s*";
        private const string _size   = "((?=\\()\\((?<size>[^\\)]*)\\)|([\\.]*)\\s*)((?=V)" + _size_2 + "|" + _size_1 + ")";

        private const string _default_value_1 = "\\s*";
        private const string _default_value_2 = "VALUE\\s*(?<default_value>[^\\.|\\s]*)([\\.]*)\\s*";
        private const string _default_value_3 = "COMP-3\\s*VALUE\\s*(?<default_value>[^\\.|\\s]*)([\\.]*)\\s*";

        private const string _default_value = "((?=C)" + _default_value_3 + "|((?=V)" + _default_value_2 + "|" + _default_value_1 + "))";

        private const string _position_1 = "(?<position>[^\\n]*)";
        private const string _position_2 = "COMP-3([\\.]*)\\s*(?<position>[^\\n]*)";
        private const string _position   = "((?=C)" + _position_2 + "|" + _position_1 + ")";

        private const string IdProperty =
            "\t\t[IgnoreDataMember]\n\t\t[XmlIgnore]\n\t\t[JsonIgnore]\n\t\t[Key]\n\t\tpublic int Id { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }";

        private static readonly string groupPattern = @"(?:^\[)(?<Group>.+)(?:\]$)";
        private static readonly string valuePattern = @"(?:^)(?<Key>.+)=(?<Value>.+)(?:$)";

        private static readonly string pattern = $@"(?(^\[){groupPattern}|{valuePattern})";

        private static readonly Regex property1ParserRegex =
            new Regex("^(?<level>[^\\s]*)\\s*(?<name>[^\\s]*)\\s*PIC\\s*(?<type>[^\\(]*)\\((?<size>[^\\)]*)\\)\\s*VALUE\\s*(?<default_value>[^\\.]*).\\s*(?<position>[^\\n]*)$",
                      RegexOptions.Compiled);

        private static readonly Regex propertyParserRegex = new Regex($"^{_level}{_name}{_type}{_size}{_default_value}{_position}$", RegexOptions.Compiled);

        private static readonly Regex constantParserRegex = new Regex("(?<level>[^\\s]*)\\s*(?<name>[^\\s]*)\\s*VALUE\\s*\'(?<default_value>[^\\n]*)\'.", RegexOptions.Compiled);

        private static readonly Func<string, string, string> PropertyTemplate = (name,
                                                                                 type) =>
            $"\t\t[DataMember]\n\t\t[XmlElement]\n\t\t[JsonProperty(nameof({name}), NamingStrategyType = typeof(DefaultNamingStrategy))]" +
            $"\n\t\tpublic {type}? {name} {{ [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }}";

        private static readonly Func<string, string> ClassTemplate = name =>
            $"\t[Serializable]\n\t[DataContract]\n\t[XmlRoot(nameof({name}))]\n\t[Table(nameof({name}))]\n\tpublic class {name}";

        private static string GetType(Property property)
        {
            if(!property.Compressed && property.VSize == 0)
            {
                if(property.Type.StartsWith("X"))
                {
                    if(property.Size == 1)
                    {
                        return "char";
                    }

                    return "string";
                }

                if(property.Type.StartsWith("9"))
                {
                    if(property.Size == 1)
                    {
                        return "sbyte";
                    }

                    if(property.Size == 2)
                    {
                        return "short";
                    }

                    if(property.Size > 2 && property.Size <= 4)
                    {
                        return "int";
                    }

                    return "long";
                }
            }
            else if(!property.Compressed && property.VSize != 0)
            {
                if(property.Size + property.VSize < 8)
                {
                    return "float";
                }

                return "double";
            }

            if(property.VSize == 0)
            {
                if(property.Size == 1)
                {
                    return "sbyte";
                }

                if(property.Size == 2)
                {
                    return "short";
                }

                if(property.Size > 2 && property.Size <= 4)
                {
                    return "int";
                }

                return "long";
            }

            if(property.Size + property.VSize < 8)
            {
                return "float";
            }

            return "double";
        }

        private static string GetTypeName(Property property)
        {
            
            if(!property.Compressed && property.VSize == 0)
            {
                if(property.Type.StartsWith("X"))
                {
                    if(property.Size == 1)
                    {
                        return "Char";
                    }

                    return "String";
                }

                if(property.Type.StartsWith("9"))
                {
                    if(property.Size == 1)
                    {
                        return "SByte";
                    }

                    if(property.Size == 2)
                    {
                        return "Int16";
                    }

                    if(property.Size > 2 && property.Size <= 4)
                    {
                        return "Int32";
                    }

                    return "Int64";
                }
            }
            else if(!property.Compressed && property.VSize != 0)
            {
                if(property.Size + property.VSize < 8)
                {
                    return "Single";
                }

                return "Double";
            }

            if(property.VSize == 0)
            {
                if(property.Size == 1)
                {
                    return "SByte";
                }

                if(property.Size == 2)
                {
                    return "Int16";
                }

                if(property.Size > 2 && property.Size <= 4)
                {
                    return "Int32";
                }

                return "Int64";
            }

            if(property.Size + property.VSize < 8)
            {
                return "Single";
            }

            return "Double";






            //if(!property.Compressed)
            //{
            //    if(property.Type.StartsWith("X"))
            //    {
            //        if(property.Size == 1)
            //        {
            //            return "Char";
            //        }

            //        return "String";
            //    }

            //    if(property.Size == 1)
            //    {
            //        return "SByte";
            //    }

            //    if(property.Size == 2)
            //    {
            //        return "Int16";
            //    }

            //    if(property.Size > 2 && property.Size <= 4)
            //    {
            //        return "Int32";
            //    }

            //    if(property.Size > 4 && property.Size <= 8)
            //    {
            //        return "Int64";
            //    }
            //}

            //if(property.VSize == 0)
            //{
            //    if(property.Size == 2)
            //    {
            //        return "Int16";
            //    }

            //    if(property.Size > 2 && property.Size <= 4)
            //    {
            //        return "Int32";
            //    }

            //    if(property.Size > 4 && property.Size <= 8)
            //    {
            //        return "Int64";
            //    }
            //}

            //if(property.Size + property.VSize < 8)
            //{
            //    return "Single";
            //}

            //return "Double";
        }

        public static string Process(string   className,
                                     string[] lines)
        {
            List<Property> Properties = new List<Property>();
            List<Constant> Constants  = new List<Constant>();

            Match match;

            string name;
            string type;
            int    size;
            int    vSize;
            string defaultValue;
            int    position;
            bool   compressed;
            bool   redefines = false;

            foreach(string line in lines)
            {
                if(redefines && line.Contains("OCCURS"))
                {
                    Properties.Add(new Property(line));

                    continue;
                }

                if(line.Length < 2 || (!line[0].IsDigit() && !line[1].IsDigit()))
                {
                    continue;
                }

                redefines = line.Contains("REDEFINES");

                if(line.StartsWith("88"))
                {
                    match = constantParserRegex.Match(line);
                }
                else
                {
                    match = propertyParserRegex.Match(line);

                    if(match.Success && !string.IsNullOrEmpty(match.Groups["position"].Value) && !match.Groups["position"].Value.Contains("."))
                    {
                        name       = match.Groups["name"].Value.Trim();
                        type       = match.Groups["type"].Value.Trim();
                        position   = int.Parse(match.Groups["position"].Value.Trim());
                        compressed = line.Contains("COMP-3");

                        if(match.Groups["size"].Success)
                        {
                            size = int.Parse(match.Groups["size"].Value.Trim());
                        }
                        else
                        {
                            size = type.Length;
                        }

                        if(match.Groups["v_size"].Success)
                        {
                            vSize = int.Parse(match.Groups["v_size"].Value.Trim());
                        }
                        else
                        {
                            vSize = 0;
                        }

                        if(match.Groups["default_value"].Success)
                        {
                            defaultValue = match.Groups["default_value"].Value.Trim();
                        }
                        else
                        {
                            defaultValue = "";
                        }

                        Properties.Add(new Property(name, type, size, vSize, defaultValue, position, compressed));

                        continue;
                    }

                    Properties.Add(new Property(line));
                }
            }

            StringBuilder sb = new StringBuilder();

            sb.Append("#nullable enable\n");
            sb.Append("using System;\n");
            sb.Append("using System.ComponentModel.DataAnnotations;\n");
            sb.Append("using System.ComponentModel.DataAnnotations.Schema;\n");
            sb.Append("using System.Runtime.CompilerServices;\n");
            sb.Append("using System.Runtime.Serialization;\n");
            sb.Append("using System.Xml.Serialization;\n");
            sb.Append("\n");
            sb.Append("using Newtonsoft.Json;\n");
            sb.Append("using Newtonsoft.Json.Serialization;\n");
            sb.Append("\n");
            sb.Append("namespace OilGas.Data.RRC.Texas\n");
            sb.Append("{\n");

            sb.Append(ClassTemplate(className));
            sb.Append("\t{\n");

            sb.Append("\n");
            sb.Append(IdProperty);
            sb.Append("\n");

            {
                foreach(Property property in Properties.Where(p => !p.Name.Contains("FILLER") && !p.Name.Contains("VALUE")))
                {
                    sb.Append(!property.Manual ? PropertyTemplate(property.Name, GetType(property)) : $"\t\t//{property.Name}\n");
                    sb.Append("\n");
                }
            }

            sb.Append($"\t\tpublic {className}()\n");
            sb.Append("\t\t{\n");
            sb.Append("\t\t}\n");
            sb.Append("\n");

            sb.Append($"\t\tpublic {className}(ReadOnlySpan<byte> source)\n");
            sb.Append("\t\t{\n");

            {
                List<Property> properties = Properties;

                Property property;
                Property nextProperty;
                int      nextIndex;

                for(int i = 0; i < properties.Count - 1; ++i)
                {
                    property     = properties[i];
                    nextIndex    = 1;
                    nextProperty = properties[i + nextIndex];

                    if(property.Name.Contains("FILLER") || property.Name.Contains("VALUE"))
                    {
                        continue;
                    }

                    while(nextProperty.Position == 0)
                    {
                        nextProperty = properties[i + ++nextIndex];
                    }

                    if(property.Manual)
                    {
                        sb.Append($"\t\t\t//{property.Name}\n");
                    }
                    else
                    {
                        string typeName = GetTypeName(property);

                        if(property.Compressed)
                        {
                            if(typeName == "Int64")
                            {
                                sb.Append($"\t\t\t{property.Name}              = StringParser.ReadAsPackedInt64(source, {property.Position} - 1, {nextProperty.Position - property.Position});\n");
                            }
                            else if(typeName == "Single")
                            {
                                sb.Append($"\t\t\t{property.Name}              = StringParser.ReadAsPackedSingle(source, {property.VSize}, {property.Position} - 1, {nextProperty.Position - property.Position});\n");
                            }
                            else if(typeName == "Double")
                            {
                                sb.Append($"\t\t\t{property.Name}              = StringParser.ReadAsPackedDouble(source, {property.VSize}, {property.Position} - 1, {nextProperty.Position - property.Position});\n");
                            }
                            else
                            {
                                sb.Append($"\t\t\t{property.Name}              = StringParser.ReadAsPackedInt32(source, {property.Position} - 1, {nextProperty.Position - property.Position});\n");
                            }
                        }
                        else
                        {
                            if(typeName == "Single")
                            {
                                sb.Append($"\t\t\t{property.Name}              = StringParser.ReadAsSingle(source, {property.VSize}, {property.Position} - 1, {nextProperty.Position - property.Position});\n");
                            }
                            else if(typeName == "Double")
                            {
                                sb.Append($"\t\t\t{property.Name}              = StringParser.ReadAsDouble(source, {property.VSize}, {property.Position} - 1, {nextProperty.Position - property.Position});\n");
                            }
                            else
                            {
                                sb.Append($"\t\t\t{property.Name}              = StringParser.ReadAs{GetTypeName(property)}(source, {property.Position} - 1, {nextProperty.Position - property.Position});\n");
                            }
                        }
                    }
                }

                property = properties[^1];

                if(!property.Name.Contains("FILLER") && !property.Name.Contains("VALUE"))
                {
                    if(property.Manual)
                    {
                        sb.Append($"\t\t\t//{property.Name}\n");
                    }
                    else
                    {
                        string typeName = GetTypeName(property);

                        if(property.Compressed)
                        {
                            if(typeName == "Int64")
                            {
                                sb.Append($"\t\t\t{property.Name}              = StringParser.ReadAsPackedInt64(source, {property.Position} - 1, {238 - property.Position});\n");
                            }
                            else if(typeName == "Single")
                            {
                                sb.Append($"\t\t\t{property.Name}              = StringParser.ReadAsPackedSingle(source, {property.VSize}, {property.Position} - 1, {238 - property.Position});\n");
                            }
                            else if(typeName == "Double")
                            {
                                sb.Append($"\t\t\t{property.Name}              = StringParser.ReadAsPackedDouble(source, {property.VSize}, {property.Position} - 1, {238 - property.Position});\n");
                            }
                            else
                            {
                                sb.Append($"\t\t\t{property.Name}              = StringParser.ReadAsPackedInt32(source, {property.Position} - 1, {238 - property.Position});\n");
                            }
                        }
                        else
                        {
                            if(typeName == "Single")
                            {
                                sb.Append($"\t\t\t{property.Name}              = StringParser.ReadAsSingle(source, {property.VSize}, {property.Position} - 1, {238 - property.Position});\n");
                            }
                            else if(typeName == "Double")
                            {
                                sb.Append($"\t\t\t{property.Name}              = StringParser.ReadAsDouble(source, {property.VSize}, {property.Position} - 1, {238 - property.Position});\n");
                            }
                            else
                            {
                                sb.Append($"\t\t\t{property.Name}              = StringParser.ReadAs{GetTypeName(property)}(source, {property.Position} - 1, {238 - property.Position});\n");
                            }
                        }
                    }
                }
            }

            sb.Append("\t\t}\n");

            sb.Append("\t}\n");

            sb.Append("}\n");

            return sb.ToString();
        }

        public sealed class Property
        {
            public string Name { get; set; }

            public string Type { get; set; }

            public int Size { get; set; }

            public int VSize { get; set; }

            public string DefaultValue { get; set; }

            public int Position { get; set; }

            public bool Compressed { get; set; }

            public bool Manual { get; set; }

            public Property(string name)
            {
                Name   = name;
                Manual = true;
            }

            public Property(string name,
                            string type,
                            int    size,
                            int    vSize,
                            string defaultValue,
                            int    position,
                            bool   compressed)
            {
                Name         = name.Replace("-", "_");
                Type         = type;
                Size         = size;
                VSize        = vSize;
                DefaultValue = defaultValue;
                Position     = position;
                Compressed   = compressed;
                Manual       = false;
            }
        }

        public sealed class Constant
        {
            public string Name { get; set; }

            public string Value { get; set; }
        }
    }
}
