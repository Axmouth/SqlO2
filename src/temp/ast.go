package gosql

type Ast struct {
	Statements []*Statement
}

type AstKind uint

const (
	SelectKind AstKind = iota
	CreateTableKind
	InsertKind
)

type Statement struct {
	SelectStatement      *SelectStatement
	CreateTableStatement *CreateTableStatement
	InsertStatement      *InsertStatement
	Kind                 AstKind
}

type InsertStatement struct {
	table  token
	values *[]*expression
}

type expressionKind uint

const (
	literalKind expressionKind = iota
)

type expression struct {
	literal *token
	kind    expressionKind
}

type columnDefinition struct {
	name     token
	datatype token
}

type CreateTableStatement struct {
	name token
	cols *[]*columnDefinition
}

type SelectStatement struct {
	item []*expression
	from token
}
