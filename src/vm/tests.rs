//! Tests for VM functionality demonstrating Result-based error handling

#[cfg(test)]
mod tests {
    use crate::vm::{VmError, VmResult};

    #[test]
    fn test_vm_error_variants() {
        // Test that our error variants exist and can be pattern matched
        let hard_error = VmError::Hard;
        let exception_error = VmError::Exception;
        
        // These should compile and demonstrate the variants exist
        assert!(matches!(hard_error, VmError::Hard));
        assert!(matches!(exception_error, VmError::Exception));
        
        // Test that they're not equal (basic differentiation)
        assert_ne!(hard_error, exception_error);
    }

    #[test] 
    fn test_vm_result_type_alias() {
        // Test that VmResult type alias works and ? operator is usable
        fn returns_vm_result() -> VmResult {
            Ok(())
        }
        
        fn takes_vm_result(result: VmResult) -> VmResult {
            result?; // Demonstrate ? operator works
            Ok(())
        }
        
        let result = returns_vm_result();
        assert!(takes_vm_result(result).is_ok());
    }

    #[test]
    fn test_error_differentiation_in_match() {
        // Test that we can differentiate between error types in match statements
        fn create_hard_error() -> VmResult {
            Err(VmError::Hard)
        }
        
        fn create_exception_error() -> VmResult {
            Err(VmError::Exception)
        }
        
        // Test Hard error differentiation
        match create_hard_error() {
            Ok(()) => panic!("Expected error"),
            Err(VmError::Hard) => {}, // This should match
            Err(VmError::Exception) => panic!("Wrong error type"),
        }
        
        // Test Exception error differentiation  
        match create_exception_error() {
            Ok(()) => panic!("Expected error"),
            Err(VmError::Hard) => panic!("Wrong error type"),
            Err(VmError::Exception) => {}, // This should match
        }
    }
}