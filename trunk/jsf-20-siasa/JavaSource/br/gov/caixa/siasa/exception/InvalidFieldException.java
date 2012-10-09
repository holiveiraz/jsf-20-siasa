package br.gov.caixa.siasa.exception;

public class InvalidFieldException extends RuntimeException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 8864915027404419584L;

	public InvalidFieldException(String message, Throwable cause) {
		super(message, cause);
	}

	public InvalidFieldException(String message) {
		super(message);
	}

	public InvalidFieldException(Throwable cause) {
		super(cause);
	}
}
