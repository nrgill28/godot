using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace Godot.SourceGenerators
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class MustBeVariantAnalyzer : DiagnosticAnalyzer
    {
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
            => ImmutableArray.Create(
                Common.GenericTypeArgumentMustBeVariantRule,
                Common.GenericTypeParameterMustBeVariantAnnotatedRule,
                Common.TypeArgumentParentSymbolUnhandledRule);

        public override void Initialize(AnalysisContext context)
        {
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.EnableConcurrentExecution();
            context.RegisterSyntaxNodeAction(AnalyzeTypeArgumentList, SyntaxKind.TypeArgumentList);
            context.RegisterSyntaxNodeAction(AnalyzeExportedProperties, SyntaxKind.FieldDeclaration);
            context.RegisterSyntaxNodeAction(AnalyzeExportedProperties, SyntaxKind.PropertyDeclaration);
            context.RegisterSyntaxNodeAction(AnalyzeSignals, SyntaxKind.DelegateDeclaration);
        }

        private void AnalyzeTypeArgumentList(SyntaxNodeAnalysisContext context)
        {
            // Ignore syntax inside comments
            if (IsInsideDocumentation(context.Node))
                return;

            var typeArgListSyntax = (TypeArgumentListSyntax)context.Node;

            // Method invocation or variable declaration that contained the type arguments
            var parentSyntax = context.Node.Parent;
            Debug.Assert(parentSyntax != null);

            var sm = context.SemanticModel;

            var typeCache = new MarshalUtils.TypeCache(context.Compilation);

            for (int i = 0; i < typeArgListSyntax.Arguments.Count; i++)
            {
                var typeSyntax = typeArgListSyntax.Arguments[i];

                // Ignore omitted type arguments, e.g.: List<>, Dictionary<,>, etc
                if (typeSyntax is OmittedTypeArgumentSyntax)
                    continue;

                var typeSymbol = sm.GetSymbolInfo(typeSyntax).Symbol as ITypeSymbol;
                Debug.Assert(typeSymbol != null);

                var parentSymbol = sm.GetSymbolInfo(parentSyntax).Symbol;

                if (!ShouldCheckTypeArgument(context, parentSyntax, parentSymbol, typeSyntax, typeSymbol, i))
                {
                    return;
                }

                if (typeSymbol is ITypeParameterSymbol typeParamSymbol)
                {
                    if (!typeParamSymbol.HasGodotMustBeVariantAttribute())
                    {
                        Common.ReportGenericTypeParameterMustBeVariantAnnotated(context, typeSyntax, typeSymbol);
                    }
                    continue;
                }

                var marshalType = MarshalUtils.ConvertManagedTypeToMarshalType(typeSymbol, typeCache);

                if (marshalType == null)
                {
                    Common.ReportGenericTypeArgumentMustBeVariant(context, typeSyntax, typeSymbol);
                    continue;
                }
            }
        }

        private void AnalyzeExportedProperties(SyntaxNodeAnalysisContext context)
        {
            // If someone uses a generic type in an exported property, it needs to be [MustBeVariant]

            SemanticModel sm = context.SemanticModel;
            var member = (MemberDeclarationSyntax)context.Node;

            if (!member.GetAllAttributes().Any(a => a.GetTypeSymbol(sm).IsGodotExportAttribute())) return;

            TypeSyntax typeSyntax = member switch
            {
                FieldDeclarationSyntax field => field.Declaration.Type,
                PropertyDeclarationSyntax property => property.Type
            };

            ITypeSymbol typeSymbol = (ITypeSymbol) sm.GetSymbolInfo(typeSyntax).Symbol!;

            if (typeSymbol is ITypeParameterSymbol typeParam && !typeParam.HasGodotMustBeVariantAttribute())
            {
                Common.ReportGenericTypeParameterMustBeVariantAnnotated(context, typeSyntax, typeSymbol);
            }
        }

        private void AnalyzeSignals(SyntaxNodeAnalysisContext context)
        {
            // If someone uses a generic type in a signal, it needs to be [MustBeVariant]

            SemanticModel sm = context.SemanticModel;
            var delegateSyntax = (DelegateDeclarationSyntax)context.Node;

            if (!delegateSyntax.GetAllAttributes().Any(a => a.GetTypeSymbol(sm).IsGodotSignalAttribute()))
                return;

            foreach (var param in delegateSyntax.ParameterList.Parameters)
            {
                if (param.Type == null) continue;

                var paramType = (ITypeSymbol) sm.GetSymbolInfo(param.Type).Symbol!;

                if (paramType is ITypeParameterSymbol typeParam && !typeParam.HasGodotMustBeVariantAttribute())
                {
                    Common.ReportGenericTypeParameterMustBeVariantAnnotated(context, param.Type, paramType);
                }
            }

            var retType = (ITypeSymbol)sm.GetSymbolInfo(delegateSyntax.ReturnType).Symbol!;
            if (retType is ITypeParameterSymbol retTypeParam && !retTypeParam.HasGodotMustBeVariantAttribute())
            {
                Common.ReportGenericTypeParameterMustBeVariantAnnotated(context, delegateSyntax.ReturnType, retType);
            }
        }

        /// <summary>
        /// Check if the syntax node is inside a documentation syntax.
        /// </summary>
        /// <param name="syntax">Syntax node to check.</param>
        /// <returns><see langword="true"/> if the syntax node is inside a documentation syntax.</returns>
        private bool IsInsideDocumentation(SyntaxNode? syntax)
        {
            while (syntax != null)
            {
                if (syntax is DocumentationCommentTriviaSyntax)
                {
                    return true;
                }

                syntax = syntax.Parent;
            }

            return false;
        }

        /// <summary>
        /// Check if the given type argument is being used in a type parameter that contains
        /// the <c>MustBeVariantAttribute</c>; otherwise, we ignore the attribute.
        /// </summary>
        /// <param name="context">Context for a syntax node action.</param>
        /// <param name="parentSyntax">The parent node syntax that contains the type node syntax.</param>
        /// <param name="parentSymbol">The symbol retrieved for the parent node syntax.</param>
        /// <param name="typeArgumentSyntax">The type node syntax of the argument type to check.</param>
        /// <param name="typeArgumentSymbol">The symbol retrieved for the type node syntax.</param>
        /// <returns><see langword="true"/> if the type must be variant and must be analyzed.</returns>
        private bool ShouldCheckTypeArgument(SyntaxNodeAnalysisContext context, SyntaxNode parentSyntax, ISymbol parentSymbol, TypeSyntax typeArgumentSyntax, ITypeSymbol typeArgumentSymbol, int typeArgumentIndex)
        {
            var typeParamSymbol = parentSymbol switch
            {
                IMethodSymbol methodSymbol => methodSymbol.TypeParameters[typeArgumentIndex],
                INamedTypeSymbol typeSymbol => typeSymbol.TypeParameters[typeArgumentIndex],
                _ => null,
            };

            if (typeParamSymbol == null)
            {
                Common.ReportTypeArgumentParentSymbolUnhandled(context, typeArgumentSyntax, parentSymbol);
                return false;
            }

            return typeParamSymbol.GetAttributes()
                .Any(a => a.AttributeClass?.IsGodotMustBeVariantAttribute() ?? false);
        }
    }
}
