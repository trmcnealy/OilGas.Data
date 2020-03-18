using System;

namespace System.Runtime.Versioning
{
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct | AttributeTargets.Method | AttributeTargets.Constructor,
                    AllowMultiple = false,
                    Inherited     = false)]
    internal sealed class NonVersionableAttribute : Attribute
    {
        public NonVersionableAttribute()
        {
        }
    }
}

[AttributeUsage(AttributeTargets.Method)]
public sealed class CallIndirectAttribute : Attribute
{
    private string _staticFieldName;

    public string StaticFieldName { get { return _staticFieldName; } set { _staticFieldName = value; } }

    public CallIndirectAttribute()
    {
        StaticFieldName = string.Empty;
    }

    public CallIndirectAttribute(string staticFieldName)
    {
        StaticFieldName = staticFieldName;
    }
}