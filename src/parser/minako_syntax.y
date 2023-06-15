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
    | functiondefinition program
    | declassignment ";" program
    ;

functiondefinition: //Check
    type id "(" parameterlist ")" "{" statementlist "}"
    | type id "(" ")" "{" statementlist "}"
    ;

parameterlist: //Check
    type id 
    | type id "," parameterlist
    ;

functioncall: //Check
    id "(" ")"
    | id "("  functioncallassignment  ")"
    ;

functioncallassignment: //Check
    assignment
    | assignment "," functioncallassignment
    ;

statementlist: //?
    %empty
    | block statementlist
    ;

block: //Check
    "{" statementlist "}"
    |	statement
    ;

statement: //Check
    ifstatement
    | forstatement
    | whilestatement
    | returnstatement ";"
    | dowhilestatement ";"
    | printf ";"
    | declassignment ";"
    | statassignment ";"
    | functioncall ";"
    ;

/*statblock:
    "{" statementlist "}"
    | statement
    ;*/

ifstatement: //Fixed
    KW_IF "(" assignment ")" block KW_ELSE block
    | KW_IF "(" assignment ")" block LOWER_THAN_ELSE
    ;

forstatement: //Check
    KW_FOR "(" declassignment ";" expr ";" statassignment ")" block
    | KW_FOR "(" statassignment ";" expr ";" statassignment ")" block
    ;

dowhilestatement: //Check
    KW_DO block KW_WHILE "(" assignment ")"
    ;

whilestatement: //Check
    KW_WHILE "(" assignment ")" block
    ;

returnstatement: //Check
    KW_RETURN
    | KW_RETURN assignment
    ;

printf: //Check
    KW_PRINTF "(" CONST_STRING ")"
    | KW_PRINTF "(" assignment ")"
    ;

declassignment: //Check
    type id
    | type id "=" assignment 
    ;

type: //Check
    KW_BOOLEAN
    | KW_FLOAT
    | KW_INT
    | KW_VOID
    ;

statassignment: //Check
    id "=" assignment
    ;

assignment:
    id "=" assignment
    | expr
    ;

expr:               //MAYBE
    simpexpr
    | simpexpr "==" simpexpr
    | simpexpr "<" simpexpr
    | simpexpr ">" simpexpr
    ;

simpexpr:
    term simpexpr2
    | "-" term simpexpr2
    ;

simpexpr2:
    %empty
    | "+" term simpexpr2
    | "-" term simpexpr2
    | "||" term simpexpr2
    ;

term:
    factor term2
    ;

term2:
    %empty
    | "*" factor term2
    | "/" factor term2
    | "&&" factor
    ;

factor:
    CONST_INT
    | CONST_FLOAT
    | CONST_BOOLEAN
    | functioncall
    | id //???
    | "(" assignment ")"
    ;

id:
    ID
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

