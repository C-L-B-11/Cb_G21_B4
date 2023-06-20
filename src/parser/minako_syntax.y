
%expect 0

%define api.parser.struct {Parser}
%define api.value.type {Value}
%define api.parser.check_debug { self.debug }

%define parse.error custom
%define parse.trace


%code use {
    // all use goes here
    use crate::{Token, C1Lexer as Lexer, Loc, Value};
}

%code parser_fields {
    errors: Vec<String>,
    /// Enables debug printing
    pub debug: bool,
}

%token
    AND           "&&"
    OR            "||"
    EQ            "=="
    NEQ           "!="
    LEQ           "<="
    GEQ           ">="
    LSS           "<"
    GRT           ">"
    KW_BOOLEAN    "bool"
    KW_DO         "do"
    KW_ELSE       "else"
    KW_FLOAT      "float"
    KW_FOR        "for"
    KW_IF         "if"
    KW_INT        "int"
    KW_PRINTF     "printf"
    KW_RETURN     "return"
    KW_VOID       "void"
    KW_WHILE      "while"
    CONST_INT     "integer literal"
    CONST_FLOAT   "float literal"
    CONST_BOOLEAN "boolean literal"
    CONST_STRING  "string literal"
    ID            "identifier"

// definition of association and precedence of operators
%left '+' '-' OR
%left '*' '/' AND
%nonassoc UMINUS


// workaround for handling dangling else
// LOWER_THAN_ELSE stands for a not existing else
%nonassoc LOWER_THAN_ELSE
%nonassoc KW_ELSE

%%

program:
    %empty
        {
            $$ = Value::None;
        }
    | functiondefinition program {$$ = Value::None;}
    | declassignment ";" program {$$ = Value::None;}
    ;

functiondefinition: //Check
    type id "(" parameterlist ")" "{" statementlist "}" {$$ = Value::None;}
    | type id "(" ")" "{" statementlist "}" {$$ = Value::None;}
    ;

parameterlist: //Check
    type id {$$ = Value::None;}
    | type id "," parameterlist {$$ = Value::None;}
    ;

functioncall: //Check
    id "(" ")" {$$ = Value::None;}
    | id "("  functioncallassignment  ")" {$$ = Value::None;}
    ;

functioncallassignment: //Check
    assignment {$$ = Value::None;}
    | assignment "," functioncallassignment {$$ = Value::None;}
    ;

statementlist: //?
    %empty
    {
        $$ = Value::None;
    }
    | block statementlist {$$ = Value::None;}
    ;

block: //Check
    "{" statementlist "}" {$$ = Value::None;}
    |	statement {$$ = Value::None;}
    ;

statement: //Check
    ifstatement {$$ = Value::None;}
    | forstatement {$$ = Value::None;}
    | whilestatement {$$ = Value::None;}
    | returnstatement ";" {$$ = Value::None;}
    | dowhilestatement ";" {$$ = Value::None;}
    | printf ";" {$$ = Value::None;}
    | declassignment ";" {$$ = Value::None;}
    | statassignment ";" {$$ = Value::None;}
    | functioncall ";" {$$ = Value::None;}
    ;

/*statblock:
    "{" statementlist "}"
    | statement
    ;*/

ifstatement: //Fixed
    KW_IF "(" assignment ")" block KW_ELSE block {$$ = Value::None;}
    | KW_IF "(" assignment ")" block LOWER_THAN_ELSE {$$ = Value::None;}
    ;

forstatement: //Check
    KW_FOR "(" declassignment ";" expr ";" statassignment ")" block {$$ = Value::None;}
    | KW_FOR "(" statassignment ";" expr ";" statassignment ")" block {$$ = Value::None;}
    ;

dowhilestatement: //Check
    KW_DO block KW_WHILE "(" assignment ")" {$$ = Value::None;}
    ;

whilestatement: //Check
    KW_WHILE "(" assignment ")" block {$$ = Value::None;}
    ;

returnstatement: //Check
    KW_RETURN {$$ = Value::None;}
    | KW_RETURN assignment {$$ = Value::None;}
    ;

printf: //Check
    KW_PRINTF "(" CONST_STRING ")" {$$ = Value::None;}
    | KW_PRINTF "(" assignment ")" {$$ = Value::None;}
    ;

declassignment: //Check
    type id {$$ = Value::None;}
    | type id "=" assignment {$$ = Value::None;}
    ;

type: //Check
    KW_BOOLEAN {$$ = Value::None;}
    | KW_FLOAT {$$ = Value::None;}
    | KW_INT {$$ = Value::None;}
    | KW_VOID {$$ = Value::None;}
    ;

statassignment: //Check
    id "=" assignment {$$ = Value::None;}
    ;

assignment:
    id "=" assignment {$$ = Value::None;}
    | expr {$$ = Value::None;}
    ;

expr: 
    simpexpr
    | simpexpr "==" simpexpr {$$ = Value::None;}
    | simpexpr "!=" simpexpr {$$ = Value::None;}
    | simpexpr "<=" simpexpr {$$ = Value::None;}
    | simpexpr ">="simpexpr {$$ = Value::None;}
    | simpexpr '<' simpexpr {$$ = Value::None;}
    | simpexpr '>' simpexpr {$$ = Value::None;}
    ;

simpexpr:
    term simpexpr2 {$$ = Value::None;}
    | "-" term simpexpr2 {$$ = Value::None;}
    ;

simpexpr2:
    %empty
    | "+" term simpexpr2 {$$ = Value::None;}
    | "-" term simpexpr2 {$$ = Value::None;}
    | "||" term simpexpr2 {$$ = Value::None;}
    ;

term:
    factor term2 {$$ = Value::None;}
    ;

term2:
    %empty
    {
        $$ = Value::None;
    }
    | "*" factor term2 
    {
        $$ = Value::None;
    }
    | "/" factor term2 
    {
        $$ = Value::None;
    }
    | "&&" factor 
    {
        $$ = Value::None;
    }
    ;

factor:
    CONST_INT 
    {
        $$ = Value::None;
    }
    | CONST_FLOAT 
    {
        $$ = Value::None;
    }
    | CONST_BOOLEAN 
    {
        $$ = Value::None;
    }
    | functioncall 
    {
        $$ = Value::None;
    }
    | id 
    {
        $$ = Value::None;
    }
    | "(" assignment ")" 
    {
        $$ = Value::None;
    }
    ;

id:
    ID 
    {
        $$ = Value::None;
    }
    ;

%%

impl Parser {
    /// "Sucess" status-code of the parser
    pub const ACCEPTED: i32 = -1;

    /// "Failure" status-code of the parser
    pub const ABORTED: i32 = -2;

    /// Constructor
    pub fn new(lexer: Lexer) -> Self {
        // This statement was added to manually remove a dead code warning for 'owned_value_at' which is auto-generated code
        Self::remove_dead_code_warning();
        Self {
            yy_error_verbose: true,
            yynerrs: 0,
            debug: false,
            yyerrstatus_: 0,
            yylexer: lexer,
            errors: Vec::new(),
        }
    }

    /// Wrapper around generated `parse` method that also
    /// extracts the `errors` field and returns it.
    pub fn do_parse(mut self) -> Vec<String> {
        self.parse();
        self.errors
    }

    /// Retrieve the next token from the lexer
    fn next_token(&mut self) -> Token {
        self.yylexer.yylex()
    }

    /// Print a syntax error and add it to the errors field
    fn report_syntax_error(&mut self, stack: &YYStack, yytoken: &SymbolKind, loc: YYLoc) {
        let token_name = yytoken.name();
        let error = format!("Unexpected token {} at {:?}", token_name, loc);
        eprintln!("Stack: {}\nError: {}", stack, error);
        self.errors.push(error);
    }

    /// Helper function that removes a dead code warning, which would otherwise interfere with the correction of a submitted
    /// solution
    fn remove_dead_code_warning() {
    	let mut stack = YYStack::new();
    	let yystate: i32 = 0;
    	let yylval: YYValue = YYValue::new_uninitialized();
    	let yylloc: YYLoc = YYLoc { begin: 0, end: 0 };
        stack.push(yystate, yylval.clone(), yylloc);
    	let _ = stack.owned_value_at(0);
    }
}

