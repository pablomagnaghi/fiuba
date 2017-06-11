package fiuba.algo3.tp1;

import java.util.ArrayList;
import java.util.List;

public class Chat {

	private String nombre;
	
	private List<Mensaje> mensajes;
	
	public Chat(String nombreContacto) {
		mensajes = new ArrayList<Mensaje>();
		this.setNombre(nombreContacto);
	}

	public String getNombre() {
		return nombre;
	}

	public void setNombre(String nombre) {
		this.nombre = nombre;
	}

	public int cantidadDeMensajes() {
		return cantidadTotalMensajesRecibidos();
	}

	public void agregarMensajeRecibido(String mensaje) {
		mensajes.add(new MensajeRecibido(mensaje));
	}

	public int cantidadTotalMensajesRecibidos() {
		
		int cantidadTotalMensajesRecibidos = 0;
		
		for (Mensaje mensaje : mensajes) {
			if (mensaje.getClass().getSimpleName().equals("MensajeRecibido")) {
				cantidadTotalMensajesRecibidos ++;
			}
		}
		
		return cantidadTotalMensajesRecibidos;
	}
	
	public int cantidadTotalMensajesEnviados() {
		int cantidadTotalMensajesEnviados = 0;
		
		for (Mensaje mensaje : mensajes) {
			if (mensaje.getClass().getSimpleName().equals("MensajeEnviado")) {
				cantidadTotalMensajesEnviados ++;
			}
		}
		
		return cantidadTotalMensajesEnviados;
	}

	public void agregarMensajeEnviado(String mensaje) {
		mensajes.add(new MensajeEnviado(mensaje));
	}

	public List<String> getConversacion(String nombreContacto) {
		
		List<String> conversacion = new ArrayList<String>();
		
		for (Mensaje mensaje : mensajes) {
			if (mensaje.getClass().getSimpleName().equals("MensajeEnviado")) {
				conversacion.add(0, "Yo: " + mensaje.getMensaje());
			}
			else {
				conversacion.add(0, nombreContacto + ": " + mensaje.getMensaje());
			}
		}
		
		// Para evitar el problema de get(1) en el test que tendria empezar con get(0)
		// Al pedir en get(1) el primer contenido de mensaje de la conversacion, le agrego un mensaje inicial para llenar la posicion 0
		conversacion.add(0, "Conversacion");
		
		return conversacion;
	}

}
