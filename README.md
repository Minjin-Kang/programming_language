## 1. 프로젝트 개요

- **프로젝트명**: 언어 확장 설계 : Fiber → X-Fiber → Fabric
- **개발 기간**: [2025.09 ~ 2025[.](http://yyyy.mm/)12]
- **수행 방식**: 프로그래밍 언어 강의 과제
- **담당 범위:** Operational Semantics 및 Interpreter 구현 전반
- **기술 스택**: Scala

---

## 2. 클라이언트 요구사항 및 나의 역할

### 💼 클라이언트 개요

> 본 프로젝트는 함수형 언어 Fiber를 기반으로, 제어 흐름과 타입 시스템을 단계적으로 확장하며 언어 이론을 코드로 구현하는 것을 목적으로 하였다. Expression을 Environment-based inpretretation을 통해 value로 평가하는 Fiber 언어 구현을 시작으로, Continuation을 이용한 고급 제어 흐름과 타입 확장으로 점진적인 확장을 구현하였다.
> 

### 📌 요구사항 요약

- Fiber
    - Environmental-based interpreter 구현
- X-Fiber
    - CPS-based interpreter로 확장
    - First-Class Continuation (Vcc) 및 예외 처리 지원
    - 구조적 예외 처리
- Fabric
    - Static Type System
    - 사용자 정의 타입 (ADT) 및 Pattern matching
    - Mutable variants 및 Store 기반 실행 모델
    - Lazy binding 지원

### 👨‍💻 담당 역할

- Fiber Direct-style Interpreter 구현
- CPS 기반 X-Fiber Interpreter 설계 및 구현
- Continuation / Handler 모델 설계
- Fabric 단계의 Type Checker 및 Store-based Interpreter 구현
- 언어 확장에 따른 Operational Semantics 분석 및 코드 반영

---

## 3. 기술 스택 및 아키텍처

| 항목 | 사용 기술 / 개념 |
| --- | --- |
| 구현 언어 | Scala |
| 실행 모델 | Environment-based |
| Interpreter | Direct-style → CPS |
| 제어 흐름 | CPS |
| 타입 시스템 | Static Typing |
| 상태 관리 | Store + Address model |

### 📐 아키텍처 특징

- Fiber
    - Value 반환 중심
    - Call stack에 의존한 제어 흐름

![image.png](attachment:11f69b9a-0fd8-421b-a480-418317905c75:image.png)

- X-Fiber
    - Continuation과 Handler 명시적 전달
    - Non-local 제어 흐름 및 예외 처리 지원

![image.png](attachment:0020fa2d-d173-4015-bbf0-b7d3d3281f51:image.png)

- Fabric
    - Store 기반 실행
    - Mutation, Lazy evaluation, ADT 처리

![image.png](attachment:286a326a-634d-4088-8d9e-122c68ed2feb:image.png)

---

## 4. 주요 기능

| 기능 | 설명 |
| --- | --- |
| 기본 연산 | Int / Boolean 연산, 조건문 |
| 자료 구조 | Tuple, List |
| 함수 | First-Class function, 
Recursive function |
| Continuation | Vcc를 통한 실행 Context capture |
| 예외 처리 | Try/Throw 기반 구조적 예외 |
| Type System | Static type checking |
| ADT | TyepDef + Variant + Match |
| Lazy evaluation | Call-by-need semantics |

---

## 5. 문제 해결 및 기술적 도전

### ✅ 문제 사례 1: CPS 환경에서 리스트 평가의 제어 흐름 손실 문제

> CPS-based Interpreter에서 함수 parameter나 tuple과 같이 여러 expression을 평가해야하는 경우, 
기존의 map 기반 평가 방식은 continuation과 handler를 보존하지 못하는 문제가 발생했다.
> 

→ 해결 방법: 리스트 평가를 별도의 evalList 함수로 분리하여, 각 expression을 순차적으로 CPS 방식으로 평가하고, 동일한 Continuation과 handler를 일관되게 전달하도록 설계하였다. 이를 통해 리스트 평가 도중 발생하는 continuation 호출이나 예외가 정확한 제어 흐름으로 전파되도록 보장하였다.

### ✅ 문제 사례 2: CPS 환경에서 예외 처리(Throw)와 Continuation 충돌 문제

> CPS-based Interpreter에서 Throw는 단순한 오류가 아니라 비국소 제어 흐름 이동이다. 그러나 continuation이 명시적으로 전달되는 환경에서는 예외 발생 시 현재 continuation을 사용할 지 try에서 저장한 continuation으로 점프해야 할 지에 대한 기준이 명확하지 않아 문제가 발생했다.
특히 Try가 중첩되거나 handler가 함수 또는 continuation일 수 있는 경우, 잘못 구현하면 제어 흐름이 꼬이거나 예외가 정상적으로 전달되지 않는 문제가 발생했다.
> 

→ 해결 방법: 예외 처리를 함수 호출이 아닌 continuation 교체 문제로 재정의하여 문제를 해결하였다. Handler를 명시적 스택 구조(SomeHandler(prev))로 모델링하여 Try 시점에 handler expression, environment, continuation을 함께 저장하고, Throw 발생 시 현재 continuation을 무시하고, 저장된 continuation으로 비국소 점프되도록 하였다. 

---

## 6. 협업 및 커뮤니케이션

- 개인 과제 형태로 수행
- 제공된 테스트 케이스 및 fuzzing 결과를 통해 semantics 오류를 반복적으로 점검하여 문제 해결
- 복잡한 제어 흐름에 대해 코드 구조를 유지하며 디버깅 중심의 개발 수행

---

## 7. 개발 결과 및 회고

### 📈 결과

- Fiber → X-Fiber → Fabric으로 이어지는 3단계 언어 확장 전부 구현 완료
- 제공된 테스트 및 fuzzing 테스트를 모두 통과하여 의미론적 정합성 검증
- Direct-style, CPS, Store-based interpreter를 모두 구현하며
    
    언어 실행 모델의 차이를 코드 수준에서 명확히 이해
    
- 제어 흐름, 타입 시스템, 상태 관리가 결합된 현실적인 언어 실행 구조 완성

### 🤔 회고

- CPS는 단순한 구현 기법이 아니라, 언어의 표현력을 근본적으로 확장하는 설계 도구임을 체감하였다.
- Continuation을 값으로 다루면서 비국소 제어 흐름과 예외 처리가 자연스럽게 통합되는 과정을 경험하였다.
- 타입 시스템과 실행 의미론을 분리 설계함으로써 실제 컴파일러 구조와 유사한 아키텍처를 이해하게 되었다.
- 특히 Fabric 단계에서 타입, 상태, 지연 평가가 상호작용하는 복잡한 의미론을 안정적으로 구현한 경험이 인상 깊었다.
