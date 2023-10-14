using Godot.Collections;

namespace Godot.SourceGenerators.Sample;

public partial class GenericExports<[MustBeVariant] T> : GodotObject
{
    [Export] public T GenericField;
    [Export] public T GenericProperty { get; set; }
    [Export] public Array<T> GenericArrayProperty { get; set; }

    [Signal]
    public delegate void GenericSignalEventHandler(GenericExports<T> self, T arg);

    public T GenericMethod(T arg)
    {
        return arg;
    }
}

public partial class CompleteTypeVector : GenericExports<Vector2>
{
}

public partial class CompleteTypeResource : GenericExports<Resource>
{
}
