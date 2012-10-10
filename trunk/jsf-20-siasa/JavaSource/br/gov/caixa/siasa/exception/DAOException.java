package br.gov.caixa.siasa.exception;

public final class DAOException extends RuntimeException {

	private static final long serialVersionUID = 3444066624086692635L;

	public DAOException(String message, Throwable cause) {
		super(message, cause);
	}

	public DAOException(String message) {
		super(message);
	}

	public DAOException(Throwable cause) {
		super(cause);
	}

}
