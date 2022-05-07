; ModuleID = 'GoSci'
source_filename = "GoSci"

@fmt_int = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt_float = private unnamed_addr constant [4 x i8] c"%g\0A\00", align 1
@fmt_char = private unnamed_addr constant [4 x i8] c"%c\0A\00", align 1
@fmt_str = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@fmt_int.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt_float.2 = private unnamed_addr constant [4 x i8] c"%g\0A\00", align 1
@fmt_char.3 = private unnamed_addr constant [4 x i8] c"%c\0A\00", align 1
@fmt_str.4 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

declare i32 @printf(i8*, ...)

declare i32 @pow(i32, i32)

define i32 @gcd(i32 %a, i32 %b) {
entry:
  %a1 = alloca i32, align 4
  store i32 %a, i32* %a1, align 4
  %b2 = alloca i32, align 4
  store i32 %b, i32* %b2, align 4
  br label %while

while:                                            ; preds = %if_end, %entry
  %a3 = load i32, i32* %a1, align 4
  %b4 = load i32, i32* %b2, align 4
  %tmp = icmp ne i32 %a3, %b4
  br i1 %tmp, label %while_body, label %while_end

while_body:                                       ; preds = %while
  %b5 = load i32, i32* %b2, align 4
  %a6 = load i32, i32* %a1, align 4
  %tmp7 = icmp slt i32 %b5, %a6
  br i1 %tmp7, label %then, label %else

then:                                             ; preds = %while_body
  %a8 = load i32, i32* %a1, align 4
  %b9 = load i32, i32* %b2, align 4
  %tmp10 = sub i32 %a8, %b9
  store i32 %tmp10, i32* %a1, align 4
  br label %if_end

else:                                             ; preds = %while_body
  %b11 = load i32, i32* %b2, align 4
  %a12 = load i32, i32* %a1, align 4
  %tmp13 = sub i32 %b11, %a12
  store i32 %tmp13, i32* %b2, align 4
  br label %if_end

if_end:                                           ; preds = %else, %then
  br label %while

while_end:                                        ; preds = %while
  %a14 = load i32, i32* %a1, align 4
  ret i32 %a14
}

define i32 @main() {
entry:
  %a = alloca i32, align 4
  %b = alloca i32, align 4
  %c = alloca i32, align 4
  store i32 1, i32* %a, align 4
  store i32 2, i32* %b, align 4
  %a1 = load i32, i32* %a, align 4
  %b2 = load i32, i32* %b, align 4
  %tmp = add i32 %a1, %b2
  store i32 %tmp, i32* %c, align 4
  %c3 = load i32, i32* %c, align 4
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt_int.1, i32 0, i32 0), i32 %c3)
  %gcd_result = call i32 @gcd(i32 8, i32 6)
  %printf4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt_int.1, i32 0, i32 0), i32 %gcd_result)
  ret i32 0
}
