// API Service exports

export { api, ApiError } from './client'
export { datasetsApi } from './datasets'
export { emgApi, fatigueApi, synergyApi } from './emg'

// Health check
import { api } from './client'

export const healthCheck = () =>
  api.get<{ status: string; version: string }>('/api/health')

// Session management
export const sessionApi = {
  create: () => api.post<{ sessionId: string }>('/api/session'),
  destroy: (sessionId: string) => api.delete(`/api/session/${sessionId}`),
  keepAlive: (sessionId: string) =>
    api.post(`/api/session/${sessionId}/keepalive`),
}
